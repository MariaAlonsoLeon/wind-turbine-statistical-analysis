# =============================================================================
# 04_fit_agreed_model.R — Modelo acordado por el grupo (MVP)
# -----------------------------------------------------------------------------
# Responsable: Álvaro
# Uso:  source("scripts/04_fit_agreed_model.R")   (~30 s)
#
# Ajusta el modelo acordado (R/12_agreed_model.R): log(Power out) en función
# de RPM (con curvatura), temperatura en la góndola y estación, sobre los
# intervalos de 10 min con producción de 2018-2022, y su análisis de
# sensibilidad. No modifica los resultados de scripts/03_fit_models.R.
#
# Entradas : data/final/swt_analysis_dataset.rds
# Salidas  : outputs/models/agreed_results.rds, agreed_main_model.rds,
#            agreed_candidate_models.rds; outputs/tables/agreed_*.csv;
#            outputs/figures/agreed_*.pdf|png
# =============================================================================

source(here::here("R", "00_utils.R"))
load_project_packages()
source_project_functions()

config <- load_config()
set.seed(config$seed)
settings <- agreed_settings(config)
response <- config$analysis$response
data <- load_analysis_data("final", config)
log_step("Conjunto de análisis: ", nrow(data), " filas")

# --- 1. Ajuste ----------------------------------------------------------------
log_step("Modelo acordado: preparación y ajuste")
results <- fit_agreed_models(data, settings, response)
rm(data); invisible(gc())
d <- results$data
model <- results$main
log_step("Intervalos de producción: ", nrow(d))

save_model(results, "agreed_results")
save_model(model, "agreed_main_model")
save_model(results$candidates, "agreed_candidate_models")

# --- 2. Inferencia ------------------------------------------------------------
log_step("Inferencia")
save_table(attr(d, "preparation_log"), "agreed_preparation_log")
save_table(dplyr::bind_rows(lapply(results$candidates, model_summary),
                            .id = "model"), "agreed_fit_candidates")
save_table(model_summary(model), "agreed_summary")
save_table(extract_coefficients(model), "agreed_coefficients")
save_table(confidence_intervals(model), "agreed_confint")
save_table(interpret_effects(model), "agreed_effects")
save_table(term_tests(model), "agreed_term_tests")
save_table(test_coefficient(model, "log_rpm_c", 3), "agreed_test_elasticity3")
save_table(elasticity_curve(model, seq(100, 250, by = 10),
                            settings$reference_rpm), "agreed_elasticity")
save_table(season_profile(model, d, settings = settings),
           "agreed_season_profile")

# --- 3. Sensibilidad: dependencia temporal, estados, respuesta, controles -----
log_step("Sensibilidad")
save_table(dependence_summary(
  results[c("main", "thinned", "random")] |>
    stats::setNames(names(results$times)), results$times),
  "agreed_dependence")
save_table(compare_fits_across_samples(list(
  "Completa" = model, "Espaciada" = results$thinned,
  "Aleatoria" = results$random)), "agreed_sensitivity_dependence")
save_table(compare_fits_across_samples(list(
  "Completa" = model, "Con año y franja" = results$controls,
  "Estado 0" = results$status0, "Power reg" = results$power_reg)),
  "agreed_sensitivity_robustness")

# --- 4. Figuras ---------------------------------------------------------------
grid <- agreed_curve_grid(model, d, settings = settings)
save_figure(plot_power_curve_fit(d, grid, response), "agreed_power_curve")
save_figure(plot_elasticity_curve(elasticity_curve(
  model, seq(min(grid$rpm), max(grid$rpm), length.out = 100),
  settings$reference_rpm)), "agreed_elasticity")
save_figure(plot_season_profile(season_profile(model, d, settings = settings)),
            "agreed_season_profile", width = 4, height = 2.6)

log_step("Modelo acordado completado.")
