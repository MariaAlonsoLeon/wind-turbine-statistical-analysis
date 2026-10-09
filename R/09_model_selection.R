# =============================================================================
# 09_model_selection.R — Simplificación y selección de variables
# -----------------------------------------------------------------------------
# Responsable: DANIEL                          ESTADO: PENDIENTE (solo interfaz)
#
# Entrada: lista de modelos de fit_candidate_models() (06_modeling.R),
# ajustados SOBRE LAS MISMAS FILAS para que sean comparables.
# =============================================================================

#' Compara modelos candidatos
#'
#' @param models Lista nombrada de modelos.
#' @return tibble: modelo, nº de parámetros, AIC, BIC, R2 ajustado (lm) o
#'   devianza (glm); y contrastes F / razón de verosimilitudes para modelos
#'   anidados (anova()).
#' TODO [Daniel]: implementar; indicar qué comparaciones son anidadas.
compare_models <- function(models) {
  not_implemented("Daniel", "compare_models()")
}

#' Selección automática de variables (paso a paso)
#'
#' @param model Modelo de partida.
#' @param direction "backward", "forward" o "both" (Tema 6: step()).
#' @param k Penalización (2 = AIC; log(n) = BIC).
#' @return modelo seleccionado.
#' TODO [Daniel]:
#'   * Aplicar step(model, direction = direction, k = k).
#'   * Con n del orden de 10^6 el AIC tiende a conservar casi todo: valorar
#'     BIC y la relevancia práctica, no solo el criterio.
#'   * Contrastar el modelo seleccionado con el diagnóstico (08).
select_model <- function(model, direction = "both", k = 2) {
  not_implemented("Daniel", "select_model()")
}
