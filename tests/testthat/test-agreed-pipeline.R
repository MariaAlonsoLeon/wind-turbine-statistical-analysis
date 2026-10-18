# Tests INTERMEDIOS del modelo acordado: encadenan preparación -> ajuste ->
# inferencia -> tablas sobre datos simulados con efectos CONOCIDOS y
# comprueban propiedades del procedimiento (recuperación de los efectos,
# confusión entre temperatura y estación, efecto del tipo de submuestra sobre
# la dependencia temporal).

pipeline_agreed_settings <- function() {
  agreed_settings(list(analysis = list(model = NULL), seed = 1))
}

#' Datos por minuto con efecto de la estación y, opcionalmente, temperatura
#' que depende de la estación (para estudiar la confusión entre ambas).
#'
#' @param season_effect Efecto en log(P) de otoño, invierno y primavera
#'   respecto al verano.
#' @param temp_by_season Si TRUE, la temperatura media cambia con la estación
#'   (verano más cálido), pero la temperatura NO afecta a la potencia.
simulate_seasonal_minutes <- function(season_effect = c(0, 0.04, 0.05),
                                      temp_by_season = FALSE, ...) {
  sim <- simulate_scada_minutes(g = 0, ...)
  season <- season_south(as.integer(format(sim$log_time, "%m")))
  eff <- c(0, season_effect)[as.integer(season)]
  producing <- sim$power_out_w > 0
  sim$power_out_w[producing] <- sim$power_out_w[producing] *
    exp(eff[producing])
  sim$power_reg_w <- sim$power_out_w * 1.01
  if (temp_by_season) {
    shift <- c(8, 4, -4, 0)[as.integer(season)]
    sim$temp_nacelle_c <- sim$temp_nacelle_c + shift
  }
  sim
}

test_that("el modelo acordado recupera los efectos de estación simulados", {
  sim <- simulate_seasonal_minutes(n_intervals = 3000, sigma = 0.1)
  d <- prepare_agreed_data(sim, pipeline_agreed_settings())
  fit <- fit_main_model(d, agreed_formulas()$a4)
  cf <- extract_coefficients(fit)
  true <- c(seasonOtoño = 0, seasonInvierno = 0.04, seasonPrimavera = 0.05)
  for (t in names(true)) {
    i <- cf$term == t
    expect_lt(abs(cf$estimate[i] - true[[t]]), 4 * cf$std_error[i])
  }
  # La temperatura no tiene efecto en la simulación: su IC debe incluir el 0
  # (o estar muy cerca de él)
  i <- cf$term == "temp_nacelle_c"
  expect_lt(abs(cf$estimate[i]), 4 * cf$std_error[i])
  # Interpretación en % de potencia (Tema 6)
  ef <- interpret_effects(fit)
  expect_equal(ef$effect[ef$term == "seasonInvierno"],
               100 * (exp(cf$estimate[cf$term == "seasonInvierno"]) - 1))
})

test_that("sin la estación, la temperatura recoge un efecto espurio de la época", {
  # La potencia depende de la estación y la temperatura cambia con ella, pero
  # la temperatura no afecta a la potencia: a3 (sin estación) atribuye a la
  # temperatura un efecto que desaparece en a4 (con estación).
  sim <- simulate_seasonal_minutes(season_effect = c(0.05, 0.12, 0.08),
                                   temp_by_season = TRUE, n_intervals = 3000,
                                   sigma = 0.08)
  d <- prepare_agreed_data(sim, pipeline_agreed_settings())
  models <- fit_candidate_models(d, agreed_formulas()[paste0("a", 1:4)])
  t3 <- extract_coefficients(models$a3)
  t4 <- extract_coefficients(models$a4)
  p3 <- t3$p_value[t3$term == "temp_nacelle_c"]
  b3 <- t3$estimate[t3$term == "temp_nacelle_c"]
  b4 <- t4$estimate[t4$term == "temp_nacelle_c"]
  se4 <- t4$std_error[t4$term == "temp_nacelle_c"]
  expect_lt(p3, 0.001)
  expect_lt(b3, 0)                       # verano: más calor y menos potencia
  expect_lt(abs(b4), 4 * se4)
  expect_lt(abs(b4), abs(b3) / 3)
})

test_that("los modelos anidados a1-a4 usan las mismas filas y su R2 no disminuye", {
  d <- prepare_agreed_data(simulate_seasonal_minutes(n_intervals = 1500),
                           pipeline_agreed_settings())
  d$temp_nacelle_c[1:5] <- NA
  models <- fit_candidate_models(d, agreed_formulas()[paste0("a", 1:4)])
  n <- vapply(models, stats::nobs, numeric(1))
  expect_true(all(n == nrow(d) - 5))
  r2 <- vapply(models, function(m) model_summary(m)$r_squared, numeric(1))
  expect_false(is.unsorted(r2))
  tab <- table_model_fit(lapply(models, model_summary),
                         added = c(a1 = "log(RPM/140)", a2 = "+ cuadrado",
                                   a3 = "+ temperatura", a4 = "+ estación"),
                         caption = "Ajuste acordado")
  expect_equal(sum(grepl("^\\|a[1-4]", tab)), 4)
})

test_that("una submuestra aleatoria no rompe la dependencia; una espaciada sí", {
  sim <- simulate_seasonal_minutes(n_intervals = 4000, phi = 0.95,
                                   spacing_hours = 1, seed = 21)
  d <- prepare_agreed_data(sim, pipeline_agreed_settings())
  f <- agreed_formulas()$a2
  full <- fit_main_model(d, f)
  thin <- thin_sample(d, every_hours = 24)
  rand <- random_subsample(d, n = nrow(thin) * 4, seed = 3)
  dep <- dependence_summary(
    list(Completa = full, Espaciada = fit_main_model(thin, f),
         Aleatoria = fit_main_model(rand, f)),
    list(Completa = d$log_time, Espaciada = thin$log_time,
         Aleatoria = rand$log_time))
  lag1 <- stats::setNames(dep$lag1, dep$sample)
  dw <- stats::setNames(dep$dw, dep$sample)
  expect_gt(lag1[["Completa"]], 0.7)
  expect_gt(lag1[["Aleatoria"]], lag1[["Espaciada"]] + 0.2)
  expect_lt(abs(lag1[["Espaciada"]]), 0.3)
  expect_lt(dw[["Aleatoria"]], dw[["Espaciada"]])
  # En la simulación los intervalos distan 1 h: la aleatoria deja muchos
  # pares a pocas horas; la espaciada, ninguno a menos de 24 h
  gap <- stats::setNames(dep$median_gap_h, dep$sample)
  expect_lt(gap[["Aleatoria"]], 24)
  expect_gte(gap[["Espaciada"]], 24)
  tab <- table_dependence(dep, caption = "Dependencia")
  expect_true(any(grepl("Aleatoria", tab)))
})

test_that("tablas de coeficientes y sensibilidad del modelo acordado", {
  d <- prepare_agreed_data(simulate_seasonal_minutes(n_intervals = 1500),
                           pipeline_agreed_settings())
  fit <- fit_main_model(d, agreed_formulas()$a4)
  tab <- table_model_coefficients(extract_coefficients(fit),
                                  interpret_effects(fit),
                                  caption = "Coeficientes acordados")
  expect_true(any(grepl("Invierno \\(vs verano\\)", tab)))
  cmp <- compare_fits_across_samples(list(
    Completa = fit,
    Espaciada = fit_main_model(thin_sample(d, 72), agreed_formulas()$a4)))
  stab <- table_sensitivity(cmp, caption = "Sensibilidad acordada")
  expect_true(any(grepl("Espaciada \\(n = ", stab)))
})
