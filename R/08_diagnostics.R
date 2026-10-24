# =============================================================================
# 08_diagnostics.R — Diagnóstico del modelo
# -----------------------------------------------------------------------------
# Responsable: DANIEL                                          ESTADO: COMPLETO
# =============================================================================

#' Residuos frente a ajustados, normalidad y homocedasticidad
diagnose_residuals <- function(model) {
  pal <- wind_palette()
  d <- data.frame(
    fitted = stats::fitted(model),
    resid = stats::rstandard(model)
  )

  p_resid <- ggplot2::ggplot(d, ggplot2::aes(fitted, resid)) +
    ggplot2::geom_point(alpha = 0.15, colour = pal[["primary"]],
                        shape = 16, size = 0.7) +
    ggplot2::geom_hline(yintercept = 0, colour = pal[["secondary"]],
                        linetype = "22", linewidth = 0.6) +
    ggplot2::geom_smooth(method = "loess", formula = y ~ x,
                         colour = pal[["secondary"]], se = FALSE,
                         linewidth = 0.7) +
    ggplot2::scale_x_continuous(labels = axis_es()) +
    ggplot2::scale_y_continuous(labels = axis_es()) +
    ggplot2::labs(x = "Valores ajustados (log W)",
                  y = "Residuos estandarizados") +
    theme_wind_report()

  p_qq <- ggplot2::ggplot(d, ggplot2::aes(sample = resid)) +
    ggplot2::stat_qq(colour = pal[["primary"]], alpha = 0.15,
                     shape = 16, size = 0.7) +
    ggplot2::stat_qq_line(colour = pal[["secondary"]], linewidth = 0.6) +
    ggplot2::scale_x_continuous(labels = axis_es()) +
    ggplot2::scale_y_continuous(labels = axis_es()) +
    ggplot2::labs(x = "Cuantiles teóricos normales",
                  y = "Residuos estandarizados") +
    theme_wind_report()

  list(
    data = tibble::as_tibble(d),
    plots = list(residuals_vs_fitted = p_resid, qq_plot = p_qq)
  )
}

#' Observaciones atípicas e influyentes
check_influential_observations <- function(model, data) {
  n <- stats::nobs(model)
  inf_data <- tibble::tibble(
    index = seq_len(n),
    log_time = data$log_time,
    status0_share = data$status0_share,
    stud_resid = stats::rstudent(model),
    leverage = stats::hatvalues(model),
    cooks_d = stats::cooks.distance(model)
  )

  inf_data$is_outlier <- abs(inf_data$stud_resid) > 3
  inf_data$is_influential <- inf_data$cooks_d > (4 / n)

  inf_data[order(-inf_data$cooks_d), ]
}

#' Multicolinealidad (VIF con R base / stats)
check_multicollinearity <- function(model) {
  X <- stats::model.matrix(model)[, -1, drop = FALSE]
  vif_vals <- vapply(seq_len(ncol(X)), function(i) {
    y <- X[, i]
    x_sub <- X[, -i, drop = FALSE]
    r2 <- summary(stats::lm(y ~ x_sub))$r.squared
    1 / (1 - r2)
  }, numeric(1))

  tibble::tibble(Termino = colnames(X), VIF = vif_vals)
}

#' Autocorrelación de los residuos (Durbin-Watson en R base)
check_autocorrelation <- function(model, data) {
  e <- stats::residuals(model)
  t_ord <- order(data$log_time)
  e_ord <- e[t_ord]
  dw_stat <- sum(diff(e_ord)^2) / sum(e_ord^2)

  pal <- wind_palette()
  d <- data.frame(time = data$log_time[t_ord], resid = stats::rstandard(model)[t_ord])

  p_time <- ggplot2::ggplot(d, ggplot2::aes(time, resid)) +
    ggplot2::geom_line(colour = pal[["neutral"]], linewidth = 0.2) +
    ggplot2::geom_hline(yintercept = 0, colour = pal[["secondary"]], linetype = "dashed") +
    ggplot2::labs(x = "Tiempo", y = "Residuos Estandarizados") +
    theme_wind_report()

  list(
    test = tibble::tibble(
      Estadistico = dw_stat,
      Metodo = "Estadístico de Durbin-Watson (orden temporal)"
    ),
    plot_time = p_time
  )
}
