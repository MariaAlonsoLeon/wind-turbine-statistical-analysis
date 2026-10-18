# Tests UNITARIOS de R/06_modeling.R: cada función por separado, con datos
# pequeños construidos a mano cuyo resultado se conoce de antemano.

default_settings <- function() model_settings(list(analysis = list(model = NULL)))

# --- Parámetros y variables auxiliares ---------------------------------------

test_that("model_settings combina config.yml con los valores por defecto", {
  s <- model_settings(list(analysis = list(model = list(interval = "hour"))))
  expect_equal(s$interval, "hour")
  expect_equal(s$min_producing_minutes, 8)
  expect_equal(default_settings()$reference_rpm, 140)
})

test_that("interval_start redondea hacia abajo al inicio del intervalo", {
  t <- as.POSIXct(c("2018-01-01 10:09:59", "2018-01-01 10:10:00"), tz = "UTC")
  expect_equal(format(interval_start(t, 600), "%H:%M"), c("10:00", "10:10"))
  expect_equal(interval_seconds("hour"), 3600)
  expect_error(interval_seconds("day"))
})

test_that("day_period asigna las cuatro franjas de 6 horas", {
  fp <- day_period(c(0, 5, 6, 11, 12, 17, 18, 23))
  expect_equal(as.character(fp),
               rep(c("0-6 h", "6-12 h", "12-18 h", "18-24 h"), each = 2))
  expect_equal(levels(fp)[1], "0-6 h")
})

test_that("season_south usa las estaciones del hemisferio sur", {
  s <- season_south(c(1, 4, 7, 10, 12))
  expect_equal(as.character(s),
               c("Verano", "Otoño", "Invierno", "Primavera", "Verano"))
})

test_that("year_factor pone el año de referencia como primer nivel", {
  f <- year_factor(c(2020, 2018, 2019), reference_year = 2019)
  expect_equal(levels(f), c("2019", "2018", "2020"))
  expect_equal(levels(year_factor(c(2020, 2021), 2018)), c("2020", "2021"))
})

# --- Conjuntos del modelo -------------------------------------------------------

# Dos intervalos de 10 min: el primero con 10 minutos con producción y el
# segundo con solo 5 (producción no sostenida); más 5 minutos parados.
toy_minutes <- function() {
  t0 <- as.POSIXct("2019-06-01 12:00:00", tz = "UTC")
  tibble::tibble(
    log_time = t0 + c(0:9, 10:19) * 60,
    power_out_w = c(seq(100, 190, by = 10), 50, 50, 50, 50, 50, 0, 0, 0, 0, 0),
    rpm = c(rep(140, 10), rep(120, 5), rep(0, 5)),
    temp_nacelle_c = c(rep(20, 10), rep(25, 10)),
    turbine_status = c(rep(0, 10), rep(8, 5), rep(1, 5))
  )
}

test_that("prepare_conditional_data agrega solo minutos con producción y filtra intervalos", {
  out <- prepare_conditional_data(toy_minutes(), default_settings())
  expect_equal(nrow(out), 1)
  expect_equal(out$n_records, 10)
  expect_equal(out$power_out_w, mean(seq(100, 190, by = 10)))
  expect_equal(out$log_power, log(145))
  expect_equal(out$log_rpm_c, 0)                   # 140 rpm = referencia
  expect_equal(out$status0_share, 1)
  expect_equal(as.character(out$day_period), "12-18 h")
  log <- attr(out, "preparation_log")
  expect_equal(log$n, c(20, 15, 15, 2, 1))
  expect_false(is.unsorted(rev(log$n[1:3])))       # nunca aumenta
})

test_that("con un umbral menor se conserva también el intervalo corto", {
  s <- default_settings(); s$min_producing_minutes <- 5
  out <- prepare_conditional_data(toy_minutes(), s)
  expect_equal(nrow(out), 2)
  expect_equal(out$power_out_w[2], 50)             # los ceros no entran en la media
})

test_that("prepare_conditional_data exige las columnas necesarias", {
  expect_error(prepare_conditional_data(toy_minutes()[, -2], default_settings()),
               "faltan columnas")
})

test_that("prepare_production_data define la respuesta binaria por intervalo", {
  s <- default_settings(); s$min_records <- 10
  out <- prepare_production_data(toy_minutes(), s)
  expect_equal(nrow(out), 2)
  expect_equal(out$n_producing, c(10, 5))
  expect_equal(out$share_producing, c(1, 0.5))
  expect_equal(out$produce, c(1L, 1L))             # 0,5 >= production_share
  s$production_share <- 0.6
  expect_equal(prepare_production_data(toy_minutes(), s)$produce, c(1L, 0L))
  expect_equal(as.character(out$season[1]), "Invierno")
})

test_that("thin_sample conserva observaciones separadas al menos k horas", {
  d <- tibble::tibble(log_time = as.POSIXct("2018-01-01", tz = "UTC") +
                        seq(0, 48 * 3600, by = 600))
  th <- thin_sample(d, every_hours = 6)
  expect_true(all(diff(as.numeric(th$log_time)) >= 6 * 3600))
  expect_equal(nrow(th), 9)                        # 0, 6, ..., 48 h
  expect_equal(th$log_time[1], min(d$log_time))
  expect_equal(nrow(thin_sample(d[sample(nrow(d)), ], 6)), 9)  # orden indiferente
})

# --- Fórmulas y ajuste -------------------------------------------------------------

test_that("las fórmulas m1-m5 son anidadas y no usan predictoras prohibidas", {
  f <- model_formulas("power_out_w")
  terms_of <- function(x) attr(stats::terms(x), "term.labels")
  for (k in 1:4) {
    expect_true(all(terms_of(f[[paste0("m", k)]]) %in%
                      terms_of(f[[paste0("m", k + 1)]])))
  }
  forbidden <- c("current_out_a", "current_amplitude", "power_reg_w",
                 "windspeed_ref_ms", "energy_wh_cum", "turbine_status")
  rhs <- unlist(lapply(f, function(x) all.vars(x[[3]])))
  expect_false(any(forbidden %in% rhs))
  expect_error(model_formulas("rpm"), "no prevista")
})

test_that("fit_main_model ajusta lm por defecto y glm con family", {
  d <- simulated_model_data()
  expect_s3_class(fit_main_model(d, model_formulas("power_out_w")$m1), "lm")
  expect_false(inherits(fit_main_model(d, log_power ~ log_rpm_c), "glm"))
  d$produce <- as.integer(d$rpm > 160)
  g <- fit_main_model(d, produce ~ temp_nacelle_c, family = binomial())
  expect_s3_class(g, "glm")
  expect_error(fit_main_model(d, log_power ~ no_existe), "faltan columnas")
})

test_that("fit_candidate_models usa las mismas filas en todos los modelos", {
  d <- simulated_model_data()
  d$temp_nacelle_c[1:7] <- NA                      # NA solo en m3-m5
  f <- model_formulas("power_out_w")[paste0("m", 1:5)]
  m <- fit_candidate_models(d, f)
  n <- vapply(m, stats::nobs, numeric(1))
  expect_true(all(n == nrow(d) - 7))
  expect_equal(attr(m, "n"), nrow(d) - 7)
  expect_error(fit_candidate_models(d, unname(f)), "nombre")
})

test_that("el modelo log-log recupera el exponente de la ley simulada", {
  d <- simulated_model_data(n_intervals = 1500, b = 3, g = -0.004)
  fit <- fit_main_model(d, log_power ~ log_rpm_c + temp_nacelle_c)
  cf <- summary(fit)$coefficients
  # Tolerancia de 4 errores estándar: el test no depende de la semilla
  expect_lt(abs(cf["log_rpm_c", 1] - 3), 4 * cf["log_rpm_c", 2])
  expect_lt(abs(cf["temp_nacelle_c", 1] + 0.004), 4 * cf["temp_nacelle_c", 2])
  expect_equal(summary(fit)$sigma, 0.15, tolerance = 0.1)
})
