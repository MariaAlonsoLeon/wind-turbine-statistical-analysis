# =============================================================================
# 07_inference.R — Inferencia e interpretación de coeficientes
# -----------------------------------------------------------------------------
# Responsable: ÁLVARO                          ESTADO: PENDIENTE (solo interfaz)
#
# Entrada común: un modelo ajustado por fit_main_model() (06_modeling.R).
# Salida esperada: tibbles que 10_tables.R pueda formatear para el informe.
#
# AVISO METODOLÓGICO: con datos SCADA cada minuto, los errores estándar de
# lm()/glm() suponen independencia. Si el grupo no corrige la dependencia
# temporal (D-M5), los p-valores e intervalos estarán probablemente
# subestimados; debe discutirse en el informe.
# =============================================================================

#' Resumen global del ajuste
#'
#' @param model Objeto lm/glm.
#' @return tibble de una fila: n, gl, R2 y R2 ajustado (lm) o devianzas y AIC
#'   (glm), estadístico F / contraste global y p-valor.
#' TODO [Álvaro]: implementar a partir de summary(model) / anova(model).
model_summary <- function(model) {
  not_implemented("Álvaro", "model_summary()")
}

#' Tabla de coeficientes
#'
#' @return tibble: term, estimate, std_error, statistic, p_value.
#' TODO [Álvaro]:
#'   * Extraer de summary(model)$coefficients.
#'   * Interpretar cada coeficiente en las unidades originales (W, rpm...),
#'     teniendo en cuenta transformaciones.
extract_coefficients <- function(model) {
  not_implemented("Álvaro", "extract_coefficients()")
}

#' Intervalos de confianza de los coeficientes
#'
#' @param level Nivel de confianza (0.95 por defecto).
#' @return tibble: term, lower, upper.
#' TODO [Álvaro]: confint(model, level = level); para glm, decidir entre
#'   intervalos de Wald o de perfil de verosimilitud.
confidence_intervals <- function(model, level = 0.95) {
  not_implemented("Álvaro", "confidence_intervals()")
}

#' Intervalos de confianza / predicción para nuevos valores
#'
#' @param newdata data.frame con los valores de las predictoras.
#' @param interval "confidence" o "prediction".
#' TODO [Álvaro]: predict(model, newdata, interval = interval); decidir si se
#'   incluye (p. ej. curva de potencia estimada con bandas).
prediction_intervals <- function(model, newdata, interval = "confidence") {
  not_implemented("Álvaro", "prediction_intervals()")
}
