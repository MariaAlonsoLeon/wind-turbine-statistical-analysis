# =============================================================================
# 02_run_eda.R — Análisis exploratorio completo (resultados internos)
# -----------------------------------------------------------------------------
# Responsable: María
# Uso:  source("scripts/02_run_eda.R")
#
# Genera TODAS las tablas (CSV) y figuras (PDF/PNG) exploratorias en
# outputs/. El informe solo incluye una selección; el resto queda disponible
# para el grupo. Usa data/processed (solo correcciones técnicas): el EDA
# describe los datos completos ANTES de cualquier filtro metodológico.
# =============================================================================

source(here::here("R", "00_utils.R"))
load_project_packages()
source_project_functions()

config <- load_config()
response <- config$analysis$response
data <- load_analysis_data("processed", config)
log_step("EDA sobre ", nrow(data), " registros; respuesta = ", response)

continuous_vars <- c(response, "power_reg_w", "rpm", "windspeed_ref_ms",
                     "voltage_in_v", "current_out_a", "voltage_l1_v",
                     "voltage_l2_v", "line_freq_hz", "temp_heatsink1_c",
                     "temp_nacelle_c")
cor_vars <- c(response, "rpm", "windspeed_ref_ms", "voltage_in_v",
              "target_tsr", "voltage_l1_v", "voltage_l2_v", "line_freq_hz",
              "line_resistance_ohm", "temp_heatsink1_c", "temp_nacelle_c")

# --- Tablas -------------------------------------------------------------------
tables <- list(
  eda_numeric_summary = numeric_summary(data, continuous_vars),
  eda_by_year = summarise_by_year(data, response),
  eda_by_turbine_status = summarise_by_status(data, "turbine_status", response),
  eda_by_grid_status = summarise_by_status(data, "grid_status", response),
  eda_by_system_status = summarise_by_status(data, "system_status", response),
  eda_power_by_windspeed = summarise_response_by_level(data, "windspeed_ref_ms",
                                                       response),
  eda_response_correlations = response_correlations(
    data, numeric_schema_columns(data), response),
  eda_extreme_values = count_extreme_values(data, continuous_vars),
  eda_daily = summarise_daily(data, response),
  eda_diurnal_profile = summarise_diurnal_profile(data, response),
  eda_candidate_variables = candidate_variables(data, response)
)
hourly <- regular_series(data, response, width_s = 3600)
minute <- regular_series(data, response, width_s = 60)
tables$eda_acf_hourly <- acf_table(hourly, lag_max = 72)
tables$eda_acf_minute <- acf_table(minute, lag_max = 120)
tables$eda_monthly <- summarise_monthly(data, response)
for (nm in names(tables)) save_table(tables[[nm]], nm)

# --- Comprobaciones específicas de la respuesta -------------------------------
power_cmp <- compare_power_measures(data, response, "power_reg_w")
high <- high_power_check(data, config$reference$rated_power_w, response)
for (nm in names(power_cmp)) save_table(power_cmp[[nm]],
                                        paste0("response_out_vs_reg_", nm))
for (nm in names(high)) save_table(high[[nm]], paste0("response_high_power_", nm))

cor_mat <- correlation_matrix(data, cor_vars)
save_table(tibble::as_tibble(cor_mat, rownames = "variable"),
           "eda_correlation_matrix")

# --- Figuras ------------------------------------------------------------------
figures <- list(
  fig_power_distribution = plot_power_distribution(data, response),
  fig_power_by_windspeed = plot_power_by_level(data, "windspeed_ref_ms",
                                               response),
  fig_power_vs_rpm = plot_power_vs_continuous(data, "rpm", response),
  fig_power_vs_voltage_in = plot_power_vs_continuous(data, "voltage_in_v",
                                                     response),
  fig_power_vs_temp_nacelle = plot_power_vs_continuous(data, "temp_nacelle_c",
                                                       response),
  fig_power_by_turbine_status = plot_power_by_status(data, "turbine_status",
                                                     response),
  fig_power_by_grid_status = plot_power_by_status(data, "grid_status",
                                                  response),
  fig_daily_series = plot_daily_series(tables$eda_daily),
  fig_monthly_series = plot_monthly_series(tables$eda_monthly),
  fig_missingness = plot_missingness(summarise_missing(data)),
  fig_correlation = plot_correlation_heatmap(cor_mat),
  fig_power_by_year = plot_power_by_year(data, response),
  fig_diurnal_profile = plot_diurnal_profile(tables$eda_diurnal_profile),
  fig_acf_hourly = plot_acf(tables$eda_acf_hourly, "horas"),
  fig_acf_minute = plot_acf(tables$eda_acf_minute, "minutos")
)
for (nm in names(figures)) save_figure(figures[[nm]], nm)

log_step("EDA terminado: ", length(tables) + 1, " tablas y ",
         length(figures), " figuras en outputs/")
