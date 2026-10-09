# =============================================================================
# 11_plots.R — Tema visual, paleta y gráficos del informe
# -----------------------------------------------------------------------------
# Responsable: María. Álvaro y Daniel: usad SIEMPRE theme_wind_report() y
# wind_palette() en vuestros gráficos (diagnóstico, efectos, etc.) y añadid
# las funciones al final de este fichero.
#
# Convenciones (válidas para todo el proyecto):
#   * Cada función devuelve un objeto ggplot; no guarda nada (save_figure()).
#   * SIN título ni pie dentro del gráfico: los gestiona knitr (fig.cap).
#   * Figuras insertadas a su tamaño real en el PDF (fig.width = ancho final),
#     para que el texto se imprima al tamaño definido aquí.
#   * Máximo tres colores funcionales (wind_palette()). Las magnitudes usan una
#     sola tonalidad de claro a oscuro; nunca paletas arcoíris.
# =============================================================================

#' Paleta del proyecto: tres colores funcionales
#'
#' * primary   (#1F4E79, azul oscuro): dato principal.
#' * secondary (#E69F00, ámbar): referencia o elemento destacado. Poco
#'   contraste sobre blanco: usar solo en marcas gruesas o discontinuas y
#'   explicarlo en el caption.
#' * neutral   (#737373, gris): elementos secundarios y contexto.
#' Comprobada para deficiencias de visión del color (separación ΔE >= 21) y
#' distinguible en escala de grises (luminosidad L* 32 / 71 / 48).
#' `neutral_light` es un tinte del gris para rellenos (no es un color nuevo).
wind_palette <- function() {
  c(primary = "#1F4E79", secondary = "#E69F00", neutral = "#737373",
    neutral_light = "#E3E3E3")
}

#' Tema gráfico estándar del proyecto
#'
#' Fondo blanco, ejes visibles, retícula principal tenue y sin secundaria,
#' tipografía sans serif a `base_size` pt, leyenda discreta y márgenes
#' uniformes. Sin títulos ni pies dentro del gráfico.
theme_wind_report <- function(base_size = 9.5) {
  ink <- "grey15"
  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = "white", colour = NA),
      panel.background = ggplot2::element_rect(fill = "white", colour = NA),
      panel.grid.major = ggplot2::element_line(colour = "grey90",
                                               linewidth = 0.25),
      panel.grid.minor = ggplot2::element_blank(),
      axis.line = ggplot2::element_line(colour = "grey35", linewidth = 0.3),
      axis.ticks = ggplot2::element_line(colour = "grey35", linewidth = 0.3),
      axis.ticks.length = ggplot2::unit(2, "pt"),
      axis.text = ggplot2::element_text(colour = ink,
                                        size = ggplot2::rel(0.9)),
      axis.title = ggplot2::element_text(colour = ink),
      strip.text = ggplot2::element_text(colour = ink, hjust = 0,
                                         size = ggplot2::rel(0.95)),
      strip.background = ggplot2::element_blank(),
      legend.position = "bottom",
      legend.title = ggplot2::element_text(colour = ink,
                                           size = ggplot2::rel(0.9)),
      legend.text = ggplot2::element_text(colour = ink,
                                          size = ggplot2::rel(0.85)),
      legend.background = ggplot2::element_rect(fill = "white",
                                                colour = NA),
      legend.margin = ggplot2::margin(1, 2, 1, 2),
      legend.box.spacing = ggplot2::unit(2, "pt"),
      plot.title = ggplot2::element_blank(),
      plot.subtitle = ggplot2::element_blank(),
      plot.caption = ggplot2::element_blank(),
      plot.margin = ggplot2::margin(4, 6, 2, 2)
    )
}

#' Etiquetas numéricas en formato español (punto de miles, coma decimal)
axis_es <- function(...) scales::label_number(big.mark = ".",
                                              decimal.mark = ",", ...)

#' Etiqueta legible para una variable (nombre original + unidad)
var_label <- function(name, schema = scada_schema()) {
  i <- match(name, schema$name)
  if (is.na(i)) return(name)
  unit <- schema$unit[i]
  if (unit %in% c("--", "No documentado", "código")) schema$original[i]
  else paste0(schema$original[i], " (", unit, ")")
}

#' Leyenda dentro del panel, compatible con ggplot2 3.4 y >= 3.5
#'
#' @param x,y Posición relativa (0-1) de la esquina indicada en `just`.
legend_inside <- function(x, y, just = c(0, 1)) {
  if (utils::packageVersion("ggplot2") >= "3.5.0") {
    ggplot2::theme(legend.position = "inside",
                   legend.position.inside = c(x, y),
                   legend.justification = just)
  } else {
    ggplot2::theme(legend.position = c(x, y), legend.justification = just)
  }
}

#' Línea de referencia horizontal o vertical (p. ej. potencia nominal)
reference_line <- function(value, axis = c("y", "x")) {
  axis <- match.arg(axis)
  col <- wind_palette()[["secondary"]]
  if (is.null(value)) return(NULL)
  if (axis == "y") {
    ggplot2::geom_hline(yintercept = value, colour = col, linewidth = 0.6,
                        linetype = "22")
  } else {
    ggplot2::geom_vline(xintercept = value, colour = col, linewidth = 0.6,
                        linetype = "22")
  }
}

# -----------------------------------------------------------------------------
# Figuras del informe
# -----------------------------------------------------------------------------

#' Distribución de la potencia en los registros con potencia positiva
#'
#' Eje y en % de los registros con potencia positiva (independiente del
#' tamaño muestral). Los registros con potencia nula se excluyen para que la
#' forma sea visible; su porcentaje debe indicarse en el caption.
#'
#' @param reference Valor de referencia opcional (p. ej. potencia nominal).
plot_power_distribution <- function(data, response = "power_out_w",
                                    binwidth = 25, reference = NULL) {
  y <- data[[response]]
  ggplot2::ggplot(data.frame(power = y[!is.na(y) & y > 0]),
                  ggplot2::aes(power)) +
    ggplot2::geom_histogram(
      ggplot2::aes(y = ggplot2::after_stat(100 * count / sum(count))),
      binwidth = binwidth, boundary = 0, fill = wind_palette()[["primary"]],
      colour = "white", linewidth = 0.15) +
    reference_line(reference, "x") +
    ggplot2::scale_x_continuous(labels = axis_es(),
                                expand = ggplot2::expansion(c(0.01, 0.02))) +
    ggplot2::scale_y_continuous(labels = axis_es(accuracy = 1),
                                expand = ggplot2::expansion(c(0, 0.05))) +
    ggplot2::labs(x = var_label(response),
                  y = "% de registros con P > 0") +
    theme_wind_report()
}

#' Curva de potencia empírica frente a un predictor discreto (cajas)
#'
#' @param ylim Límites comunes del eje y (para alinear con otro panel).
#' @param show_n Añade el nº de registros bajo cada nivel.
plot_power_by_level <- function(data, predictor = "windspeed_ref_ms",
                                response = "power_out_w", show_n = FALSE,
                                ylim = NULL, reference = NULL) {
  pal <- wind_palette()
  d <- data.frame(level = factor(data[[predictor]]), power = data[[response]])
  counts <- table(d$level)
  labels <- if (show_n) {
    paste0(names(counts), "\n(", fmt_int(as.numeric(counts)), ")")
  } else names(counts)
  ggplot2::ggplot(d, ggplot2::aes(level, power)) +
    reference_line(reference, "y") +
    ggplot2::geom_boxplot(outlier.size = 0.25, outlier.alpha = 0.4,
                          outlier.colour = pal[["neutral"]],
                          fill = pal[["neutral_light"]], colour = "grey25",
                          linewidth = 0.3, width = 0.7) +
    ggplot2::stat_summary(fun = mean, geom = "point", shape = 18, size = 2,
                          colour = pal[["primary"]]) +
    ggplot2::scale_x_discrete(labels = labels) +
    ggplot2::scale_y_continuous(labels = axis_es()) +
    ggplot2::coord_cartesian(ylim = ylim) +
    ggplot2::labs(x = var_label(predictor), y = var_label(response)) +
    theme_wind_report()
}

#' Respuesta frente a un predictor continuo (histograma bidimensional)
#'
#' Con cientos de miles de puntos un diagrama de dispersión se satura; se
#' representa el nº de registros por celda en escala logarítmica, con una
#' sola tonalidad (gris claro -> azul oscuro). La leyenda va dentro del panel,
#' a la izquierda y justo por debajo de la línea de referencia (zona sin
#' datos: velocidad de giro baja con potencia alta), para que el panel
#' conserve el mismo ancho que su compañero en figuras de dos paneles.
plot_power_vs_continuous <- function(data, predictor = "rpm",
                                     response = "power_out_w", bins = 60,
                                     ylim = NULL, reference = NULL) {
  pal <- wind_palette()
  d <- data.frame(x = data[[predictor]], y = data[[response]])
  y_top <- if (is.null(ylim)) max(d$y, na.rm = TRUE) else ylim[2]
  legend_top <- if (is.null(reference)) 0.98 else
    max(0.3, min(0.98, reference / y_top - 0.05))
  ggplot2::ggplot(d, ggplot2::aes(x, y)) +
    reference_line(reference, "y") +
    ggplot2::geom_bin2d(bins = bins) +
    ggplot2::scale_fill_gradient(low = pal[["neutral_light"]],
                                 high = pal[["primary"]], trans = "log10",
                                 labels = axis_es(),
                                 breaks = function(lim) {
                                   10^seq(0, floor(log10(lim[2])), by = 2)
                                 },
                                 name = "Registros") +
    ggplot2::scale_x_continuous(labels = axis_es()) +
    ggplot2::scale_y_continuous(labels = axis_es()) +
    ggplot2::coord_cartesian(ylim = ylim) +
    ggplot2::labs(x = var_label(predictor), y = var_label(response)) +
    theme_wind_report() +
    legend_inside(0.03, legend_top) +
    ggplot2::theme(
      legend.direction = "horizontal",
      legend.background = ggplot2::element_blank(),
      legend.key.width = ggplot2::unit(0.75, "cm"),
      legend.key.height = ggplot2::unit(0.2, "cm")
    ) +
    ggplot2::guides(fill = ggplot2::guide_colourbar(title.position = "top"))
}

#' Serie mensual: potencia media y cobertura de registros (dos paneles)
#'
#' Los nombres de los paneles actúan como títulos del eje y (a la izquierda),
#' de modo que el caption no necesita repetirlos. Los meses sin registros
#' quedan sin barra en la potencia y a 0 en la cobertura.
#'
#' @param bar_width Anchura de las barras en días (24 para meses).
plot_monthly_series <- function(monthly, bar_width = 24) {
  pal <- wind_palette()
  long <- rbind(
    data.frame(month = monthly$month, value = monthly$mean_power,
               panel = "Potencia (W)"),
    data.frame(month = monthly$month, value = monthly$coverage_pct,
               panel = "Cobertura (%)")
  )
  long$panel <- factor(long$panel, levels = unique(long$panel))
  ggplot2::ggplot(long, ggplot2::aes(month, value, fill = panel)) +
    ggplot2::geom_col(width = bar_width, na.rm = TRUE, show.legend = FALSE) +
    ggplot2::facet_wrap(~panel, ncol = 1, scales = "free_y",
                        strip.position = "left") +
    ggplot2::scale_fill_manual(values = unname(pal[c("primary", "neutral")])) +
    ggplot2::scale_x_date(date_breaks = "6 months",
                          labels = scales::label_date_short(),
                          expand = c(0.01, 0)) +
    ggplot2::scale_y_continuous(labels = axis_es(),
                                expand = ggplot2::expansion(c(0, 0.06))) +
    ggplot2::labs(x = NULL, y = NULL) +
    theme_wind_report() +
    ggplot2::theme(strip.placement = "outside",
                   strip.text.y.left = ggplot2::element_text(angle = 90,
                                                             hjust = 0.5),
                   panel.spacing = ggplot2::unit(6, "pt"))
}

#' Función de autocorrelación muestral con bandas aproximadas al 95 %
#'
#' @param acf_tbl Salida de acf_table() (05_eda.R).
#' @param lag_unit Unidad del retardo para el eje x.
#' @param break_every Separación de las marcas del eje x (24 h -> cada 6).
plot_acf <- function(acf_tbl, lag_unit = "horas", break_every = 6) {
  pal <- wind_palette()
  ci <- acf_tbl$ci[1]
  d <- acf_tbl[acf_tbl$lag > 0, ]
  ggplot2::ggplot(d, ggplot2::aes(lag, acf)) +
    ggplot2::annotate("rect", xmin = -Inf, xmax = Inf, ymin = -ci, ymax = ci,
                      fill = pal[["neutral_light"]], alpha = 0.7) +
    ggplot2::geom_hline(yintercept = 0, colour = "grey35", linewidth = 0.3) +
    ggplot2::geom_segment(ggplot2::aes(xend = lag, yend = 0),
                          colour = pal[["primary"]], linewidth = 0.6) +
    ggplot2::scale_x_continuous(breaks = seq(0, max(d$lag), break_every),
                                expand = ggplot2::expansion(c(0.01, 0.01))) +
    ggplot2::scale_y_continuous(labels = axis_es(accuracy = 0.1)) +
    ggplot2::labs(x = paste0("Retardo (", lag_unit, ")"),
                  y = "Autocorrelación") +
    theme_wind_report() +
    ggplot2::theme(panel.grid.major.x = ggplot2::element_blank())
}

# -----------------------------------------------------------------------------
# Figuras auxiliares (solo outputs/figures; no están en el informe)
# -----------------------------------------------------------------------------

#' Potencia según código de estado (códigos con al menos min_n registros)
plot_power_by_status <- function(data, status_var = "turbine_status",
                                 response = "power_out_w", min_n = 100) {
  pal <- wind_palette()
  counts <- table(data[[status_var]])
  keep <- names(counts)[counts >= min_n]
  d <- data.frame(code = as.character(data[[status_var]]),
                  power = data[[response]])
  d <- d[d$code %in% keep, ]
  d$code <- factor(d$code, levels = names(sort(counts[keep],
                                               decreasing = TRUE)))
  labels <- paste0(levels(d$code), "\n(n=", fmt_int(as.numeric(
    counts[levels(d$code)])), ")")
  ggplot2::ggplot(d, ggplot2::aes(code, power)) +
    ggplot2::geom_boxplot(outlier.size = 0.25, outlier.alpha = 0.4,
                          outlier.colour = pal[["neutral"]],
                          fill = pal[["neutral_light"]], linewidth = 0.3) +
    ggplot2::scale_x_discrete(labels = labels) +
    ggplot2::scale_y_continuous(labels = axis_es()) +
    ggplot2::labs(x = paste("Código de", var_label(status_var)),
                  y = var_label(response)) +
    theme_wind_report()
}

#' Serie diaria: potencia media y cobertura (dos paneles)
plot_daily_series <- function(daily) {
  monthly_like <- data.frame(month = daily$date, mean_power = daily$mean_power,
                             coverage_pct = daily$coverage_pct)
  plot_monthly_series(monthly_like, bar_width = 1)
}

#' Porcentaje de valores perdidos por variable
plot_missingness <- function(missing) {
  missing$variable <- stats::reorder(missing$variable, missing$pct_missing)
  ggplot2::ggplot(missing, ggplot2::aes(pct_missing, variable)) +
    ggplot2::geom_col(fill = wind_palette()[["neutral"]]) +
    ggplot2::labs(x = "% de valores perdidos", y = NULL) +
    theme_wind_report()
}

#' Mapa de calor de una matriz de correlaciones (divergente: dos tonos y
#' blanco en el 0)
plot_correlation_heatmap <- function(cor_matrix) {
  pal <- wind_palette()
  d <- as.data.frame(as.table(cor_matrix))
  names(d) <- c("x", "y", "r")
  lab <- vapply(levels(d$x), var_label, character(1))
  ggplot2::ggplot(d, ggplot2::aes(x, y, fill = r)) +
    ggplot2::geom_tile(colour = "white") +
    ggplot2::geom_text(ggplot2::aes(label = fmt_num(r, 2)), size = 2.3,
                       colour = "grey10") +
    ggplot2::scale_fill_gradient2(low = pal[["secondary"]], mid = "white",
                                  high = pal[["primary"]], limits = c(-1, 1),
                                  name = "Spearman") +
    ggplot2::scale_x_discrete(labels = lab) +
    ggplot2::scale_y_discrete(labels = lab) +
    ggplot2::labs(x = NULL, y = NULL) +
    theme_wind_report() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 40, hjust = 1),
                   axis.line = ggplot2::element_blank(),
                   panel.grid.major = ggplot2::element_blank())
}

#' Potencia por año (solo registros con potencia positiva)
plot_power_by_year <- function(data, response = "power_out_w") {
  pal <- wind_palette()
  y <- data[[response]]
  d <- data.frame(year = factor(data$year), power = y)
  pct <- tapply(y > 0, d$year, mean, na.rm = TRUE) * 100
  labels <- paste0(names(pct), "\n(", fmt_num(pct), " % > 0)")
  ggplot2::ggplot(d[!is.na(y) & y > 0, ], ggplot2::aes(year, power)) +
    ggplot2::geom_boxplot(outlier.size = 0.25, outlier.alpha = 0.4,
                          outlier.colour = pal[["neutral"]],
                          fill = pal[["neutral_light"]], linewidth = 0.3) +
    ggplot2::scale_x_discrete(labels = labels) +
    ggplot2::scale_y_continuous(labels = axis_es()) +
    ggplot2::labs(x = NULL, y = var_label(response)) +
    theme_wind_report()
}

#' Perfil diario medio (hora local de reloj)
plot_diurnal_profile <- function(profile) {
  col <- wind_palette()[["primary"]]
  ggplot2::ggplot(profile, ggplot2::aes(hour, mean_power)) +
    ggplot2::geom_line(colour = col, linewidth = 0.6) +
    ggplot2::geom_point(size = 1.2, colour = col) +
    ggplot2::scale_x_continuous(breaks = seq(0, 23, 3)) +
    ggplot2::scale_y_continuous(labels = axis_es()) +
    ggplot2::labs(x = "Hora local", y = "Potencia media (W)") +
    theme_wind_report()
}
