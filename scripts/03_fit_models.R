# =============================================================================
# 03_fit_models.R — Ajuste, inferencia, diagnóstico y selección
# -----------------------------------------------------------------------------
# Responsables: Álvaro (pasos 1-2) y Daniel (pasos 3-4)
# ESTADO: PENDIENTE. El esqueleto se ejecuta sin error y avisa de lo que
# falta; descomentar cada paso a medida que se implemente.
#
# Entradas : data/final/swt_analysis_dataset.rds
# Salidas  : outputs/models/*.rds, outputs/tables/model_*.csv
# =============================================================================

source(here::here("R", "00_utils.R"))
load_project_packages()
source_project_functions()

config <- load_config()
set.seed(config$seed)
data <- load_analysis_data("final", config)
log_step("Conjunto de análisis: ", nrow(data), " filas")

# --- 1. Modelos candidatos y modelo principal (Álvaro) ------------------------
# formulas <- model_formulas(config$analysis$response)
# models   <- fit_candidate_models(data, formulas)
# model    <- fit_main_model(data, formulas$main)
# save_model(model, "main_model")

# --- 2. Inferencia (Álvaro) ---------------------------------------------------
# save_table(model_summary(model), "model_summary")
# save_table(extract_coefficients(model), "model_coefficients")
# save_table(confidence_intervals(model), "model_confint")

# --- 3. Diagnóstico (Daniel) --------------------------------------------------
# diagnostics <- diagnose_residuals(model)
# influential <- check_influential_observations(model, data)
# vif_table   <- check_multicollinearity(model)
# autocorr    <- check_autocorrelation(model, data)

# --- 4. Selección (Daniel) ----------------------------------------------------
# comparison <- compare_models(models)
# selected   <- select_model(model, direction = "both")
# save_model(selected, "selected_model")

log_step("Pasos de modelización pendientes (TODO Álvaro / Daniel).")
