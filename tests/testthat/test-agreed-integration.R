# Tests de INTEGRACIÓN del modelo acordado: funciona sobre la salida real de
# las fases anteriores (lectura, limpieza y filtros de María), es coherente
# con el modelo de 06_modeling.R, que no se modifica, y la orquestación de
# scripts/04_fit_agreed_model.R es correcta. Si existen los datos reales y
# los resultados del script, se comprueba además que son reproducibles.

integration_agreed_settings <- function(...) {
  s <- agreed_settings(list(analysis = list(model = NULL), seed = 1))
  utils::modifyList(s, list(...))
}

test_that("CSV originales (fixtures) -> limpieza -> periodo -> modelo acordado", {
  clean <- clean_technical(load_all_years(fixture_files()))
  final <- add_features(build_analysis_dataset(clean,
                                               period_start = "2018-01-01"))
  d <- prepare_agreed_data(final,
                           integration_agreed_settings(min_producing_minutes = 1))
  expect_true(all(d$year == 2018))
  # Los fixtures de 2018 son de diciembre: verano austral
  expect_true(all(d$season == "Verano"))
  expect_equal(sum(d$n_records),
               sum(final$power_out_w > 0 & final$rpm > 0))
  fit <- fit_main_model(d, agreed_formulas()$a1)
  expect_equal(stats::nobs(fit), nrow(d))
})

test_that("fit_agreed_models devuelve los modelos que usan el script y el informe", {
  sim <- simulate_scada_minutes(n_intervals = 1500, years = c(2018, 2019, 2020))
  s <- integration_agreed_settings(thinning_hours = 48, random_n = 300)
  res <- fit_agreed_models(sim, s)
  expect_named(res, c("settings", "data", "candidates", "main", "controls",
                      "thinned", "random", "status0", "power_reg", "times"))
  expect_named(res$candidates, paste0("a", 1:4))
  expect_identical(stats::coef(res$main), stats::coef(res$candidates$a4))
  expect_equal(stats::nobs(res$random), 300)
  # Cada vector de tiempos corresponde, en orden, a los residuos de su modelo
  expect_equal(lengths(res$times),
               c(Completa = stats::nobs(res$main),
                 Espaciada = stats::nobs(res$thinned),
                 Aleatoria = stats::nobs(res$random)))
  expect_false(is.unsorted(res$times$Espaciada))
  expect_false(is.unsorted(res$times$Aleatoria))
  # Resúmenes de inferencia sobre todos los modelos
  for (m in res[c("main", "controls", "thinned", "random", "status0",
                  "power_reg")]) {
    expect_s3_class(model_summary(m), "tbl_df")
    expect_s3_class(term_tests(m), "tbl_df")
  }
  dep <- dependence_summary(
    stats::setNames(res[c("main", "thinned", "random")], names(res$times)),
    res$times)
  expect_equal(dep$sample, c("Completa", "Espaciada", "Aleatoria"))
  # El modelo principal coincide con ajustarlo directamente con lm()
  direct <- stats::lm(agreed_formulas()$a4, data = res$data)
  expect_equal(stats::coef(res$main), stats::coef(direct))
})

test_that("el modelo acordado es coherente con el de 06_modeling.R", {
  sim <- simulate_scada_minutes(n_intervals = 1200, years = c(2018, 2019))
  s <- integration_agreed_settings(thinning_hours = 48, random_n = 200)
  old <- fit_all_models(sim, s)
  new <- fit_agreed_models(sim, s)
  # Misma población y mismos intervalos
  expect_equal(new$data$log_time, old$conditional_data$log_time)
  expect_equal(new$data$log_power, old$conditional_data$log_power)
  # La parte común de las secuencias anidadas es idéntica (m2 = a2)
  expect_equal(stats::coef(new$candidates$a2), stats::coef(old$candidates$m2))
  # La submuestra espaciada es la misma en ambos módulos
  expect_equal(stats::nobs(new$thinned), stats::nobs(old$thinned))
})

test_that("scripts/04_fit_agreed_model.R solo llama a funciones existentes", {
  path <- here::here("scripts", "04_fit_agreed_model.R")
  pd <- utils::getParseData(parse(path, keep.source = TRUE))
  calls <- unique(pd$text[pd$token == "SYMBOL_FUNCTION_CALL"])
  missing <- calls[!vapply(calls, exists, logical(1))]
  expect_equal(missing, character(0))
})

test_that("los resultados reales del modelo acordado se reproducen (si existen)", {
  config <- load_config()
  final_path <- project_path("final_dir", config$outputs$final_file)
  results_path <- project_path("models_dir", "agreed_results.rds")
  skip_if_not(file.exists(final_path), "No existe data/final (sin CSV)")
  skip_if_not(file.exists(results_path),
              "No existen resultados: ejecutar scripts/04_fit_agreed_model.R")
  saved <- readRDS(results_path)
  refit <- fit_agreed_models(readRDS(final_path), agreed_settings(config),
                             config$analysis$response)
  expect_identical(stats::coef(refit$main), stats::coef(saved$main))
  expect_identical(stats::coef(refit$random), stats::coef(saved$random))
  # Decisiones del grupo: periodo, población y sin filtro por estado
  d <- saved$data
  expect_true(min(d$log_time) >= as.POSIXct("2018-01-01", tz = "UTC"))
  expect_true(all(d$power_out_w > 0))
  expect_true(any(d$status0_share < 1))
  expect_setequal(levels(d$season), c("Verano", "Otoño", "Invierno",
                                      "Primavera"))
})
