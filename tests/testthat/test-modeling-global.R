# Tests GLOBALES (de extremo a extremo) con los datos reales 2018-2022.
# Se omiten si no existen data/final ni los resultados de
# scripts/03_fit_models.R (p. ej. en un ordenador sin los CSV originales).
# El render del informe es lento: solo se ejecuta si la variable de entorno
# RUN_REPORT_TEST vale "true".

config <- load_config()
final_path <- project_path("final_dir", config$outputs$final_file)
results_path <- project_path("models_dir", "model_results.rds")

skip_without_real_data <- function() {
  skip_if_not(file.exists(final_path), "No existe data/final (sin CSV)")
  skip_if_not(file.exists(results_path),
              "No existen resultados: ejecutar scripts/03_fit_models.R")
}

test_that("los resultados guardados se reproducen exactamente desde data/final", {
  skip_without_real_data()
  saved <- readRDS(results_path)
  data <- readRDS(final_path)
  refit <- fit_all_models(data, model_settings(config),
                          config$analysis$response)
  expect_identical(stats::coef(refit$main), stats::coef(saved$main))
  expect_identical(stats::coef(refit$production),
                   stats::coef(saved$production))
  expect_identical(nrow(refit$conditional_data), nrow(saved$conditional_data))
})

test_that("el conjunto del modelo respeta las decisiones de config.yml", {
  skip_without_real_data()
  res <- readRDS(results_path)
  cond <- res$conditional_data
  s <- model_settings(config)
  expect_true(all(cond$n_records >= s$min_producing_minutes))
  expect_true(all(cond$power_out_w > 0 & cond$rpm > 0))
  expect_true(min(cond$log_time) >= as.POSIXct(config$analysis$period_start,
                                               tz = "UTC"))
  expect_equal(levels(cond$year_f)[1], as.character(s$reference_year))
  log <- attr(cond, "preparation_log")
  expect_false(is.unsorted(rev(log$n[log$unit == "minutos"])))
  expect_true(all(res$production_data$n_records >= s$min_records))
})

test_that("el modelo principal es coherente en todas las muestras de sensibilidad", {
  skip_without_real_data()
  res <- readRDS(results_path)
  s <- model_summary(res$main)
  expect_gt(s$r_squared, 0.8)
  expect_equal(s$n, nrow(res$conditional_data))
  # La curva de potencia es creciente y cóncava en log-log en todas las
  # muestras (elasticidad positiva que disminuye con las RPM)
  for (m in res[c("main", "thinned", "status0", "power_reg")]) {
    b <- stats::coef(m)
    expect_gt(b[["log_rpm_c"]], 0)
    expect_lt(b[["I(log_rpm_c^2)"]], 0)
  }
  # La submuestra espaciada (ya ordenada en el tiempo) tiene residuos casi
  # incorrelados; la muestra completa, no
  th <- res$thinned
  expect_lt(abs(residual_lag1(th, seq_len(stats::nobs(th)))), 0.1)
  expect_gt(residual_lag1(res$main, res$conditional_data$log_time), 0.2)
})

test_that("scripts/03 ha guardado todas las tablas y figuras del modelo", {
  skip_without_real_data()
  tables <- c("model_summary", "model_coefficients", "model_confint",
              "model_effects", "model_term_tests", "model_test_elasticity3",
              "model_elasticity", "model_sensitivity", "model_fit_candidates",
              "production_summary", "production_odds_ratios",
              "production_term_tests", "model_power_curve")
  paths <- project_path("tables_dir", paste0(tables, ".csv"))
  expect_true(all(file.exists(paths)))
  cf <- readr::read_csv(paths[2], show_col_types = FALSE)
  expect_named(cf, c("term", "estimate", "std_error", "statistic", "p_value"))
  figs <- project_path("figures_dir",
                       paste0(c("model_power_curve", "model_elasticity",
                                "model_production_profile"), ".pdf"))
  expect_true(all(file.exists(figs)))
})

test_that("el informe se genera sin cajas pendientes de Álvaro", {
  skip_without_real_data()
  skip_if_not(identical(Sys.getenv("RUN_REPORT_TEST"), "true"),
              "Render del informe solo con RUN_REPORT_TEST=true")
  rmd <- here::here("reports", "actividad1.Rmd")
  # Los avisos "`!h' float specifier changed" de LaTeX son informativos
  # (fig.pos = "!htb" del informe) y no indican ningún error.
  out <- suppressWarnings(rmarkdown::render(
    rmd, envir = new.env(parent = globalenv()), quiet = TRUE,
    output_options = list(keep_tex = TRUE)))
  base <- sub("[.]pdf$", "", out)
  on.exit(unlink(c(out, paste0(base, c(".tex", ".log", ".knit.md")),
                   paste0(base, "_files")), recursive = TRUE))
  expect_true(file.exists(out))
  tex <- readLines(paste0(base, ".tex"), encoding = "UTF-8")
  expect_false(any(grepl("\\\\pendiente\\{Álvaro\\}", tex)))
})
