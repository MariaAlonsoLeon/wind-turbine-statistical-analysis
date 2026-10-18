# Tests UNITARIOS de R/12_agreed_model.R (modelo acordado por el grupo).
# Cada función por separado, con datos pequeños o simulados cuyo resultado
# se conoce de antemano.

agreed_test_settings <- function(...) {
  s <- agreed_settings(list(analysis = list(model = NULL), seed = 1))
  utils::modifyList(s, list(...))
}

# --- Parámetros y datos ---------------------------------------------------------

test_that("agreed_settings añade tamaño y semilla de la submuestra aleatoria", {
  s <- agreed_settings(list(analysis = list(model = NULL), seed = 99))
  expect_equal(s$random_n, 5000)
  expect_equal(s$seed, 99)
  expect_equal(s$reference_rpm, 140)                 # hereda model_settings()
  s2 <- agreed_settings(list(analysis = list(agreed = list(random_n = 300)),
                             seed = 1))
  expect_equal(s2$random_n, 300)
})

test_that("prepare_agreed_data añade la estación y conserva el conjunto condicional", {
  sim <- simulate_scada_minutes()
  s <- agreed_test_settings()
  d <- prepare_agreed_data(sim, s)
  base <- prepare_conditional_data(sim, s)
  expect_equal(nrow(d), nrow(base))
  expect_equal(d$log_power, base$log_power)
  expect_s3_class(d$season, "factor")
  expect_equal(as.character(d$season), as.character(season_south(d$month)))
  expect_equal(attr(d, "preparation_log"), attr(base, "preparation_log"))
})

test_that("las fórmulas a1-a5 son anidadas y a4 es el modelo acordado", {
  f <- agreed_formulas()
  expect_named(f, paste0("a", 1:5))
  terms_of <- function(x) attr(stats::terms(x), "term.labels")
  for (k in 1:4) {
    expect_true(all(terms_of(f[[k]]) %in% terms_of(f[[k + 1]])))
  }
  expect_setequal(terms_of(f$a4), c("log_rpm_c", "I(log_rpm_c^2)",
                                    "temp_nacelle_c", "season"))
  forbidden <- c("current_out_a", "current_amplitude", "power_reg_w",
                 "windspeed_ref_ms", "energy_wh_cum", "turbine_status")
  rhs <- unlist(lapply(f, function(x) all.vars(x[[3]])))
  expect_false(any(forbidden %in% rhs))
})

test_that("random_subsample devuelve n filas distintas, ordenadas y reproducibles", {
  d <- tibble::tibble(log_time = as.POSIXct("2018-01-01", tz = "UTC") +
                        sample(1:1e6, 500) * 60, id = 1:500)
  r1 <- random_subsample(d, n = 100, seed = 7)
  r2 <- random_subsample(d, n = 100, seed = 7)
  expect_equal(nrow(r1), 100)
  expect_false(anyDuplicated(r1$id) > 0)
  expect_false(is.unsorted(r1$log_time))
  expect_identical(r1, r2)
  expect_true(all(r1$id %in% d$id))
  expect_equal(nrow(random_subsample(d, n = 900, seed = 7)), 500)
  expect_error(random_subsample(d[, "id"], 10), "faltan columnas")
})

test_that("random_subsample funciona con data.frame de una sola columna", {
  d <- data.frame(log_time = as.POSIXct("2018-01-01", tz = "UTC") + 1:50)
  expect_s3_class(random_subsample(d, n = 10, seed = 1), "data.frame")
  expect_equal(nrow(random_subsample(d, n = 10, seed = 1)), 10)
  expect_s3_class(random_subsample(d, n = 99), "data.frame")
})

test_that("random_subsample con semilla no altera el generador de la sesión", {
  d <- data.frame(log_time = as.POSIXct("2018-01-01", tz = "UTC") + 1:50)
  set.seed(123)
  expected <- stats::runif(3)
  set.seed(123)
  invisible(random_subsample(d, n = 10, seed = 1))
  expect_equal(stats::runif(3), expected)
})

# --- Dependencia temporal ---------------------------------------------------------

ar_fit <- function(phi, n = 3000, seed = 2) {
  set.seed(seed)
  x <- stats::rnorm(n)
  e <- if (phi == 0) stats::rnorm(n) else
    as.numeric(stats::arima.sim(list(ar = phi), n))
  stats::lm(y ~ x, data.frame(y = 1 + x + e, x = x))
}

test_that("durbin_watson coincide con la fórmula y con lmtest::dwtest", {
  fit <- ar_fit(0.5)
  t <- seq_len(stats::nobs(fit))
  e <- stats::residuals(fit)
  expect_equal(durbin_watson(fit, t), sum(diff(e)^2) / sum(e^2))
  skip_if_not_installed("lmtest")
  expect_equal(durbin_watson(fit, t),
               unname(lmtest::dwtest(fit)$statistic))
})

test_that("durbin_watson ~ 2 sin autocorrelación y ~ 2(1 - r1) con ella", {
  white <- ar_fit(0)
  ar <- ar_fit(0.8)
  t <- seq_len(3000)
  expect_equal(durbin_watson(white, t), 2, tolerance = 0.1)
  dw <- durbin_watson(ar, t)
  expect_lt(dw, 0.6)
  expect_equal(dw, 2 * (1 - residual_lag1(ar, t)), tolerance = 0.01)
  # Ordena por el tiempo: invertir el orden temporal no cambia el estadístico
  expect_equal(durbin_watson(ar, rev(t)), dw)
  expect_error(durbin_watson(ar, t[-1]), "distinta longitud")
})

test_that("dependence_summary resume separación, autocorrelación y DW", {
  fit <- ar_fit(0.8, n = 100)
  t_close <- as.POSIXct("2018-01-01", tz = "UTC") + (0:99) * 600
  t_far <- as.POSIXct("2018-01-01", tz = "UTC") + (0:99) * 6 * 3600
  dep <- dependence_summary(list(A = fit, B = fit), list(A = t_close, B = t_far))
  expect_named(dep, c("sample", "n", "median_gap_h", "pct_gap_1h", "lag1", "dw"))
  expect_equal(dep$median_gap_h, c(1 / 6, 6))
  expect_equal(dep$pct_gap_1h, c(100, 0))
  expect_equal(dep$n, c(100, 100))
  expect_error(dependence_summary(list(A = fit), list(B = t_far)),
               "mismos nombres")
})

# --- Efecto de la época del año -------------------------------------------------

test_that("season_profile coincide con el cálculo manual a partir de los coeficientes", {
  d <- prepare_agreed_data(simulate_scada_minutes(n_intervals = 800),
                           agreed_test_settings())
  fit <- fit_main_model(d, agreed_formulas()$a4)
  prof <- season_profile(fit, d, rpm = 140, settings = agreed_test_settings())
  b <- stats::coef(fit)
  temp <- stats::median(d$temp_nacelle_c)
  manual <- b[["(Intercept)"]] + b[["temp_nacelle_c"]] * temp +
    c(0, b[["seasonOtoño"]], b[["seasonInvierno"]], b[["seasonPrimavera"]])
  expect_equal(as.character(prof$season), levels(d$season))
  expect_equal(prof$fit, manual)
  expect_equal(prof$fit_w, exp(manual))
  expect_true(all(prof$lower_w < prof$fit_w & prof$fit_w < prof$upper_w))
  # Sin rpm se usa la velocidad de referencia (140 rpm)
  expect_equal(season_profile(fit, d, settings = agreed_test_settings())$fit,
               prof$fit)
  expect_equal(season_profile(fit, d, rpm = 200,
                              settings = agreed_test_settings())$rpm,
               rep(200, 4))
})

test_that("agreed_curve_grid crece con las RPM y anida las bandas", {
  d <- prepare_agreed_data(simulate_scada_minutes(n_intervals = 800),
                           agreed_test_settings())
  fit <- fit_main_model(d, agreed_formulas()$a4)
  g <- agreed_curve_grid(fit, d, n = 25, settings = agreed_test_settings())
  expect_equal(nrow(g), 25)
  expect_named(g, c("rpm", "fit_w", "conf_lower_w", "conf_upper_w",
                    "pred_lower_w", "pred_upper_w"))
  expect_false(is.unsorted(g$fit_w))
  expect_true(all(g$pred_lower_w <= g$conf_lower_w &
                    g$conf_upper_w <= g$pred_upper_w))
})

# --- Tablas, figuras y lectura de resultados --------------------------------------

test_that("table_dependence y plot_season_profile siguen el estilo común", {
  dep <- tibble::tibble(sample = c("Completa", "Espaciada"), n = c(100, 10),
                        median_gap_h = c(0.17, 6.5), pct_gap_1h = c(93.4, 0),
                        lag1 = c(0.23, 0.04), dw = c(1.54, 1.92))
  tab <- table_dependence(dep, caption = "Dependencia")
  expect_true(any(grepl("Dependencia", tab)))
  expect_true(any(grepl("1,54", tab)))
  prof <- tibble::tibble(season = factor(c("Verano", "Invierno")),
                         fit_w = c(110, 115), lower_w = c(109, 114),
                         upper_w = c(111, 116))
  p <- plot_season_profile(prof)
  expect_s3_class(p, "ggplot")
  expect_null(p$labels$caption)
})

test_that("load_agreed_results avisa si no existen los resultados", {
  dir <- tempfile("agreed_")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE))
  expect_error(load_agreed_results(dir, build_if_missing = FALSE),
               "04_fit_agreed_model")
  saveRDS(list(main = 1), file.path(dir, "agreed_results.rds"))
  expect_equal(load_agreed_results(dir, build_if_missing = FALSE)$main, 1)
})
