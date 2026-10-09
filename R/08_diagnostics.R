# =============================================================================
# 08_diagnostics.R — Diagnóstico del modelo
# -----------------------------------------------------------------------------
# Responsable: DANIEL                          ESTADO: PENDIENTE (solo interfaz)
#
# Entrada común: el modelo de fit_main_model() y el conjunto de análisis.
# Ninguna función debe ELIMINAR observaciones: devuelven diagnósticos. Si el
# grupo decide excluir casos influyentes, se hace con una función explícita
# y se registra en docs/decisions.md (con el modelo antes y después).
#
# Herramientas del curso (Tema 6): rstandard(), qqnorm(), ks.test(),
# car::crPlots(), cooks.distance(), hatvalues(), car::influenceIndexPlot(),
# car::vif(), lmtest::dwtest(). Si se usan car / lmtest, añadirlos a
# project_packages() (00_utils.R) y ejecutar renv::snapshot().
#
# Presentación: tablas con style_report_table() (R/10_tables.R) y gráficos con
# theme_wind_report() + wind_palette() (R/11_plots.R), ver README.
# =============================================================================

#' Residuos frente a ajustados, normalidad y homocedasticidad
#'
#' @return lista con tibbles (residuos estandarizados, contrastes) y gráficos.
#' TODO [Daniel]:
#'   * Residuos estandarizados vs ajustados y vs cada predictora.
#'   * QQ-plot y contraste de normalidad (con n muy grande cualquier
#'     contraste rechaza: priorizar el diagnóstico gráfico).
#'   * Gráficos de componente + residuo (linealidad).
diagnose_residuals <- function(model) {
  not_implemented("Daniel", "diagnose_residuals()")
}

#' Observaciones atípicas e influyentes
#'
#' @return tibble con índice, log_time, residuo estudentizado, leverage y
#'   distancia de Cook, para revisar los casos (no los elimina).
#' TODO [Daniel]:
#'   * Umbrales justificados (p. ej. |r| > 3, Cook > 4/n).
#'   * Comparar con las incidencias de calidad (huecos, códigos de estado)
#'     antes de considerar que un caso es erróneo.
check_influential_observations <- function(model, data) {
  not_implemented("Daniel", "check_influential_observations()")
}

#' Multicolinealidad (VIF, índice de condición)
#'
#' TODO [Daniel]: car::vif(model); el EDA anticipa colinealidad fuerte entre
#'   rpm, windspeed_ref_ms, voltage_in_v y min_v_from_rpm_v.
check_multicollinearity <- function(model) {
  not_implemented("Daniel", "check_multicollinearity()")
}

#' Autocorrelación de los residuos
#'
#' @param data Conjunto de análisis (para ordenar por log_time y respetar los
#'   huecos: segment_id).
#' TODO [Daniel]:
#'   * Residuos frente al tiempo y ACF de residuos (puede reutilizarse
#'     regular_series() + acf_table() de 05_eda.R).
#'   * Durbin-Watson (lmtest::dwtest) como contraste complementario.
#'   * El EDA ya muestra autocorrelación fuerte y ciclo diario en la potencia.
check_autocorrelation <- function(model, data) {
  not_implemented("Daniel", "check_autocorrelation()")
}
