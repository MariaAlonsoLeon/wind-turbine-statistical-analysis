# =============================================================================
# 06_modeling.R — Formulación y ajuste del modelo
# -----------------------------------------------------------------------------
# Responsable: ÁLVARO                          ESTADO: PENDIENTE (solo interfaz)
#
# Entrada común: el conjunto de análisis
#     data <- load_analysis_data("final")
# que ya incorpora los filtros metodológicos de config.yml (sección analysis).
# La variable respuesta está en load_config()$analysis$response.
#
# Decisiones pendientes que afectan a este fichero (ver docs/decisions.md):
#   D-M1 Respuesta: Power out vs Power reg (correlación 0,9995 en 2017).
#   D-M2 Periodo de estudio.
#   D-M3 Estados operativos incluidos (los códigos no están documentados;
#        en el EDA, turbine_status = 0 concentra casi toda la producción).
#   D-M4 Agregación temporal (minuto / 10 min / hora).
#   D-M5 Tratamiento de la dependencia temporal.
#   D-M6 Población de análisis (DECISIÓN CENTRAL): toda la potencia, P > 0 u
#        operacional (67,7 % de ceros en 2017-2022).
#   D-M7 Tipo de modelo (lm / glm) y forma funcional (relación claramente no
#        lineal potencia-RPM en el EDA).
#
# Presentación: tablas con style_report_table() (R/10_tables.R) y gráficos con
# theme_wind_report() + wind_palette() (R/11_plots.R), ver README.
# =============================================================================

#' Error estándar para funciones aún no implementadas
not_implemented <- function(owner, what) {
  stop(sprintf("%s: pendiente de implementación por %s.", what, owner),
       call. = FALSE)
}

#' Fórmula(s) candidatas del modelo
#'
#' @return lista nombrada de objetos `formula`.
#' TODO [Álvaro]:
#'   * Proponer fórmulas candidatas justificadas por el EDA (sección 3).
#'   * Decidir transformaciones (p. ej. de la respuesta o de RPM) y posibles
#'     interacciones; justificar cada una.
#'   * Evitar predictoras que son consecuencia de la respuesta
#'     (current_out_a, current_amplitude, power_reg_w...): ver
#'     variable_roles() en 05_eda.R.
#'   * Recordar que windspeed_ref_ms se calcula a partir de RPM.
model_formulas <- function(response = load_config()$analysis$response) {
  not_implemented("Álvaro", "model_formulas()")
}

#' Ajusta el modelo principal
#'
#' @param data Conjunto de análisis (load_analysis_data("final")).
#' @param formula Fórmula elegida (de model_formulas()).
#' @param family NULL para lm(); un objeto family para glm().
#' @return objeto "lm" o "glm".
#' TODO [Álvaro]:
#'   * Definir fórmula final del modelo.
#'   * Justificar variables e interacciones.
#'   * Ajustar lm/glm según decisión del grupo (D-M7).
fit_main_model <- function(data, formula = NULL, family = NULL) {
  not_implemented("Álvaro", "fit_main_model()")
}

#' Ajusta varios modelos candidatos con los mismos datos
#'
#' @param data Conjunto de análisis.
#' @param formulas Lista nombrada de fórmulas.
#' @return lista nombrada de modelos ajustados (misma muestra en todos, para
#'   que sean comparables en 09_model_selection.R).
#' TODO [Álvaro]: implementar; comprobar que no hay NA distintos entre
#'   modelos (usar las mismas filas).
fit_candidate_models <- function(data, formulas, family = NULL) {
  not_implemented("Álvaro", "fit_candidate_models()")
}

#' Guarda un modelo ajustado en outputs/models (utilidad, ya operativa)
save_model <- function(model, name, dir = project_path("models_dir")) {
  ensure_dir(dir)
  path <- file.path(dir, paste0(name, ".rds"))
  saveRDS(model, path)
  invisible(path)
}
