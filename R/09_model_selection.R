# =============================================================================
# 09_model_selection.R — Simplificación y selección de variables
# -----------------------------------------------------------------------------
# Responsable: DANIEL                                          ESTADO: COMPLETO
# =============================================================================

#' Compara modelos candidatos
compare_models <- function(models) {
  metrics <- do.call(rbind, lapply(names(models), function(nm) {
    m <- models[[nm]]
    s <- summary(m)
    tibble::tibble(
      Modelo = nm,
      Parametros = length(stats::coef(m)),
      AIC = stats::AIC(m),
      BIC = stats::BIC(m),
      R2_ajustado = s$adj.r.squared
    )
  }))

  anova_res <- do.call(stats::anova, unname(models))
  metrics$p_valor_anova <- c(NA, anova_res$`Pr(>F)`[-1])

  tibble::as_tibble(metrics)
}

#' Selección automática de variables (paso a paso)
select_model <- function(model, direction = "both", k = 2) {
  n <- stats::nobs(model)
  if (missing(k) && n > 1000) {
    k <- log(n)
    message("Utilizando penalización BIC (k = log(n)) debido al alto tamaño muestral.")
  }

  # Solución al error de entorno: inyectamos el data frame interno del modelo
  # directamente en la llamada para que step() no busque la palabra 'data' fuera.
  model$call$data <- stats::model.frame(model)

  stats::step(model, direction = direction, k = k, trace = 0)
}
