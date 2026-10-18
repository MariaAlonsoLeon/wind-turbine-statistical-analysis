# Tests de INTEGRACIÓN: el código del modelo funciona sobre la salida real de
# las fases anteriores (lectura, limpieza, variables derivadas y filtros de
# María) y la orquestación que usa scripts/03_fit_models.R es coherente.

integration_settings <- function(...) {
  s <- model_settings(list(analysis = list(model = NULL)))
  utils::modifyList(s, list(...))
}

test_that("CSV originales (fixtures) -> limpieza -> conjunto de análisis -> modelo", {
  clean <- clean_technical(load_all_years(fixture_files()))
  final <- add_features(build_analysis_dataset(clean))
  cond <- prepare_conditional_data(final,
                                   integration_settings(min_producing_minutes = 1))
  # Todos los minutos con producción quedan en algún intervalo
  producing <- final$power_out_w > 0 & final$rpm > 0
  expect_equal(sum(cond$n_records), sum(producing))
  expect_equal(attr(cond, "preparation_log")$n[1], nrow(final))
  expect_true(all(cond$power_out_w > 0))
  expect_true(all(is.finite(cond$log_power) & is.finite(cond$log_rpm_c)))
  fit <- fit_main_model(cond, log_power ~ log_rpm_c)
  expect_s3_class(fit, "lm")
  expect_equal(stats::nobs(fit), nrow(cond))
  expect_equal(nrow(extract_coefficients(fit)), 2)
})

test_that("el filtro de periodo de María y la preparación del modelo se combinan bien", {
  clean <- clean_technical(load_all_years(fixture_files()))
  final <- add_features(build_analysis_dataset(clean, period_start = "2018-01-01"))
  cond <- prepare_conditional_data(final,
                                   integration_settings(min_producing_minutes = 1))
  expect_true(all(cond$year == 2018))
  expect_equal(levels(cond$year_f), "2018")
})

test_that("fit_all_models devuelve todos los modelos que usan el script y el informe", {
  sim <- simulate_scada_minutes(n_intervals = 1200, years = c(2018, 2019, 2020))
  res <- fit_all_models(sim, integration_settings(thinning_hours = 48))
  expect_named(res, c("settings", "conditional_data", "production_data",
                      "candidates", "main", "poly3", "thinned", "status0",
                      "power_reg", "production", "production_thinned"))
  expect_identical(stats::coef(res$main), stats::coef(res$candidates$m5))
  n <- vapply(res[c("main", "thinned", "status0")], stats::nobs, numeric(1))
  expect_lt(n[["thinned"]], n[["main"]])
  expect_lte(n[["status0"]], n[["main"]])
  # Todas las funciones de inferencia funcionan sobre cada modelo
  for (m in res[c("main", "thinned", "status0", "power_reg", "production")]) {
    expect_s3_class(model_summary(m), "tbl_df")
    expect_s3_class(confidence_intervals(m), "tbl_df")
    expect_s3_class(term_tests(m), "tbl_df")
  }
  # En la simulación Power reg = 1,01 * Power out: mismo exponente
  expect_equal(stats::coef(res$power_reg)[["log_rpm_c"]],
               stats::coef(res$main)[["log_rpm_c"]], tolerance = 1e-8)
})

test_that("scripts/03_fit_models.R solo llama a funciones existentes", {
  path <- here::here("scripts", "03_fit_models.R")
  pd <- utils::getParseData(parse(path, keep.source = TRUE))
  calls <- unique(pd$text[pd$token == "SYMBOL_FUNCTION_CALL"])
  missing <- calls[!vapply(calls, exists, logical(1))]
  expect_equal(missing, character(0))
})

test_that("guardar y leer resultados del modelo conserva el objeto", {
  dir <- tempfile("models_")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE))
  fit <- fit_main_model(simulated_model_data(), log_power ~ log_rpm_c)
  path <- save_model(list(main = fit), "model_results", dir = dir)
  expect_true(file.exists(path))
  back <- load_model_results(dir, build_if_missing = FALSE)
  expect_equal(stats::coef(back$main), stats::coef(fit))
  expect_error(load_model_results(file.path(dir, "vacio"),
                                  build_if_missing = FALSE),
               "03_fit_models")
})
