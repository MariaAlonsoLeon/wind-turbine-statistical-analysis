# Tests INTERMEDIOS: encadenan varios módulos (06 -> 07 -> 10/11) sobre datos
# simulados y comprueban propiedades estadísticas del procedimiento completo
# (cobertura de los intervalos de confianza, efecto de la dependencia
# temporal), no solo el resultado de una función aislada.

pipeline_settings <- function() model_settings(list(analysis = list(model = NULL)))

test_that("datos -> modelos candidatos -> inferencia -> tablas del informe", {
  d <- simulated_model_data(n_intervals = 900, years = c(2018, 2019, 2020))
  models <- fit_candidate_models(d, model_formulas("power_out_w")[paste0("m", 1:5)])
  main <- models$m5
  # El R2 nunca disminuye al añadir términos a modelos anidados
  r2 <- vapply(models, function(m) model_summary(m)$r_squared, numeric(1))
  expect_false(is.unsorted(r2))
  cf <- extract_coefficients(main)
  ef <- interpret_effects(main)
  expect_setequal(ef$term, cf$term[-1])
  tab <- table_model_coefficients(cf, ef, caption = "Coeficientes")
  expect_true(any(grepl("Coeficientes", tab)))
  expect_true(any(grepl("Temperatura en la góndola", tab)))
  fit_tab <- table_model_fit(lapply(models, model_summary),
                             added = stats::setNames(paste("t", 1:5),
                                                     paste0("m", 1:5)),
                             caption = "Ajuste")
  expect_equal(sum(grepl("^\\|m[1-5]", fit_tab)), 5)
})

test_that("las figuras del modelo siguen el estilo común del proyecto", {
  d <- simulated_model_data()
  main <- fit_main_model(d)
  grid <- power_curve_grid(main, d, n = 30, settings = pipeline_settings())
  p1 <- plot_power_curve_fit(d, grid)
  p2 <- plot_elasticity_curve(elasticity_curve(main, grid$rpm))
  for (p in list(p1, p2)) {
    expect_s3_class(p, "ggplot")
    expect_null(p$labels$caption)
    expect_s3_class(p$theme$plot.caption, "element_blank")
  }
})

test_that("modelo de producción: datos -> glm -> perfil -> tabla y figura", {
  sim <- simulate_scada_minutes(n_intervals = 1500, share_stopped = 0.5)
  prod <- prepare_production_data(sim, pipeline_settings())
  expect_true(all(prod$produce %in% c(0L, 1L)))
  fit <- fit_production_model(prod)
  expect_s3_class(fit, "glm")
  prof <- production_profile(fit, prod)
  expect_equal(nrow(prof), 4 * nlevels(prod$season))
  expect_true(all(prof$lower >= 0 & prof$upper <= 1))
  tab <- table_odds_ratios(extract_coefficients(fit), interpret_effects(fit),
                           caption = "Odds")
  expect_true(any(grepl("Odds", tab)))
  expect_s3_class(plot_production_profile(prof), "ggplot")
})

test_that("los IC al 95 % del exponente cubren el valor verdadero ~95 % de las veces", {
  # Pequeño estudio de simulación (Monte Carlo) del procedimiento completo:
  # simular -> agregar -> ajustar -> confint. Con errores independientes la
  # cobertura empírica debe estar cerca del nivel nominal.
  covered <- vapply(1:150, function(r) {
    d <- simulated_model_data(n_intervals = 150, b = 3, seed = 100 + r)
    ci <- confidence_intervals(fit_main_model(d, log_power ~ log_rpm_c +
                                                temp_nacelle_c))
    i <- ci$term == "log_rpm_c"
    ci$lower[i] < 3 && 3 < ci$upper[i]
  }, logical(1))
  expect_gt(mean(covered), 0.89)
  expect_lt(mean(covered), 0.995)
})

test_that("con errores autocorrelacionados la submuestra espaciada reduce la dependencia", {
  d <- simulated_model_data(n_intervals = 2000, phi = 0.9,
                            spacing_hours = 1, seed = 11)
  f <- log_power ~ log_rpm_c + temp_nacelle_c
  full <- fit_main_model(d, f)
  thin <- thin_sample(d, every_hours = 24)
  thin_fit <- fit_main_model(thin, f)
  expect_gt(residual_lag1(full, d$log_time), 0.5)
  expect_lt(abs(residual_lag1(thin_fit, thin$log_time)), 0.3)
  # El estimador sigue siendo insesgado, pero con menos observaciones
  # efectivas el error estándar honesto es mayor
  cmp <- compare_fits_across_samples(list(Completa = full, Espaciada = thin_fit))
  se <- cmp$std_error[cmp$term == "log_rpm_c"]
  expect_gt(se[2], se[1])
  tab <- table_sensitivity(cmp, caption = "Sensibilidad")
  expect_true(any(grepl("Espaciada \\(n = ", tab)))
})
