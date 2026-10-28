# =============================================================================
# 03_fit_models.R — Ajuste, inferencia, diagnóstico y selección
# -----------------------------------------------------------------------------
# Responsables: Álvaro (pasos 1-2) y Daniel (pasos 3-4)
# ESTADO: pasos implementados; (descomentar cada
# paso a medida que se implemente).
#
# Entradas : data/final/swt_analysis_dataset.rds
# Salidas  : outputs/models/*.rds, outputs/tables/model_*.csv,
#            outputs/figures/model_*.pdf|png
# =============================================================================

source(here::here("R", "00_utils.R"))
load_project_packages()
source_project_functions()

config <- load_config()
set.seed(config$seed)
settings <- model_settings(config)
response <- config$analysis$response
data <- load_analysis_data("final", config)
log_step("Conjunto de análisis: ", nrow(data), " filas")

# --- 1. Modelos candidatos y modelo principal (Álvaro) ------------------------
log_step("Preparación de los datos del modelo y ajuste")
results <- fit_all_models(data, settings, response)
rm(data); invisible(gc())

cond  <- results$conditional_data
prod  <- results$production_data
model <- results$main
log_step("Modelo condicional: ", nrow(cond), " intervalos; producción: ",
         nrow(prod), " intervalos")

save_model(results, "model_results")
save_model(model, "main_model")
save_model(results$candidates, "candidate_models")
save_model(results$production, "production_model")

# --- 2. Inferencia (Álvaro) ---------------------------------------------------
log_step("Inferencia")
save_table(attr(cond, "preparation_log"), "model_preparation_log")
save_table(attr(prod, "preparation_log"), "model_production_preparation_log")
save_table(dplyr::bind_rows(lapply(results$candidates, model_summary),
                            .id = "model"), "model_fit_candidates")
save_table(model_summary(model), "model_summary")
save_table(extract_coefficients(model), "model_coefficients")
save_table(confidence_intervals(model), "model_confint")
save_table(interpret_effects(model), "model_effects")
save_table(term_tests(model), "model_term_tests")
save_table(test_coefficient(model, "log_rpm_c", 3), "model_test_elasticity3")
save_table(elasticity_curve(model, seq(100, 250, by = 10),
                            settings$reference_rpm), "model_elasticity")
save_table(compare_fits_across_samples(list(
  "Completa" = model, "Espaciada" = results$thinned,
  "Estado 0" = results$status0, "Power reg" = results$power_reg)),
  "model_sensitivity")
save_table(model_summary(results$production), "production_summary")
save_table(extract_coefficients(results$production), "production_coefficients")
save_table(interpret_effects(results$production), "production_odds_ratios")
save_table(term_tests(results$production), "production_term_tests")

grid <- power_curve_grid(model, cond, settings = settings)
save_table(grid, "model_power_curve")
save_figure(plot_power_curve_fit(cond, grid, response), "model_power_curve")
save_figure(plot_elasticity_curve(elasticity_curve(
  model, seq(min(grid$rpm), max(grid$rpm), length.out = 100),
  settings$reference_rpm)), "model_elasticity")
save_figure(plot_production_profile(production_profile(results$production,
                                                       prod)),
            "model_production_profile")

# --- 3. Diagnóstico (Daniel) --------------------------------------------------
log_step("Diagnóstico")
diagnostics <- diagnose_residuals(model)
influential <- check_influential_observations(model, cond)
vif_table   <- check_multicollinearity(model)
autocorr    <- check_autocorrelation(model, cond)

save_figure(diagnostics$plots$residuals_vs_fitted, "model_diagnostics_resid")
save_figure(diagnostics$plots$qq_plot, "model_diagnostics_qq")
save_figure(autocorr$plot_time, "model_diagnostics_autocorr")
save_table(vif_table, "model_diagnostics_vif")
save_table(autocorr$test, "model_diagnostics_dw")

# --- 4. Selección (Daniel) ----------------------------------------------------
log_step("Selección")
models     <- results$candidates
comparison <- compare_models(models)
selected   <- select_model(model, direction = "both")

save_table(comparison, "model_selection_comparison")
save_model(selected, "selected_model")
log_step("Modelización completada.")
