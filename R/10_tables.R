# =============================================================================
# 10_tables.R — Estilo único de tablas y tablas del informe
# -----------------------------------------------------------------------------
# Responsable: María. Álvaro y Daniel: construid vuestras tablas (coeficientes,
# bondad de ajuste, VIF...) con style_report_table() y añadidlas al final.
#
# Convenciones (válidas para todo el proyecto):
#   * booktabs, sin líneas verticales, cabeceras en texto normal.
#   * Números formateados ANTES de la tabla con fmt_num()/fmt_int():
#     punto de miles y coma decimal. Decimales:
#       - porcentajes: 1 decimal;      - medias y DT: 1 decimal;
#       - correlaciones: 2 (Pearson Power out/Power reg: 4);
#       - valores observados: los de la variable (0 si es entera, 1 si no).
#   * Unidades en la cabecera entre paréntesis: "Media (W)".
#   * Caption breve; definiciones y aclaraciones en la NOTA al pie.
#   * Caption y nota en texto plano (se escapan automáticamente).
# =============================================================================

#' Escapa los caracteres especiales de LaTeX en un texto plano
escape_caption <- function(x) gsub("([_%&#])", "\\\\\\1", x)

#' ¿Parece un número formateado? (para alinear a la derecha)
looks_numeric <- function(x) {
  x <- x[!is.na(x) & x != "" & x != "--"]
  length(x) > 0 && all(grepl("^[-+]?[0-9.,]+( ?%)?$", x))
}

#' Estilo estándar de las tablas del informe
#'
#' @param df Data frame con los valores YA formateados (texto).
#' @param caption Título breve de la tabla (texto plano).
#' @param note Nota al pie opcional (texto plano): definiciones, fuentes.
#' @param align Alineación por columna; por defecto, derecha si la columna es
#'   numérica y la izquierda en caso contrario.
#' @param widths Vector con nombre (nº de columna -> ancho, p. ej.
#'   c(`1` = "6cm")) para columnas de texto largo; activa el ajuste de línea.
#' @param font_size Tamaño de letra en pt.
#' @return Objeto kableExtra (LaTeX) o tabla Markdown fuera de PDF.
style_report_table <- function(df, caption, note = NULL, align = NULL,
                               widths = NULL, font_size = 8.5) {
  latex <- knitr::is_latex_output()
  if (is.null(align)) {
    align <- vapply(df, function(col) if (looks_numeric(col)) "r" else "l",
                    character(1))
  }
  if (!latex) {
    tab <- knitr::kable(df, format = "pipe", caption = caption, align = align)
    if (!is.null(note)) attr(tab, "note") <- note
    return(tab)
  }
  tab <- knitr::kable(df, format = "latex", booktabs = TRUE, linesep = "",
                      caption = escape_caption(caption), align = align,
                      escape = TRUE)
  tab <- kableExtra::kable_styling(tab, latex_options = "hold_position",
                                   font_size = font_size, full_width = FALSE,
                                   position = "center")
  for (col in names(widths)) {
    tab <- kableExtra::column_spec(tab, as.integer(col), width = widths[[col]])
  }
  if (!is.null(note)) {
    tab <- kableExtra::footnote(tab, general = note, general_title = "Nota:",
                                footnote_as_chunk = TRUE,
                                threeparttable = TRUE, escape = TRUE)
  }
  tab
}

#' Formatea una fecha-hora de reloj (sin zona) para tablas
fmt_time <- function(x) format(x, "%Y-%m-%d %H:%M")

# -----------------------------------------------------------------------------
# Tablas del informe (María)
# -----------------------------------------------------------------------------

#' Registros, cobertura y huecos por fichero anual
table_annual_coverage <- function(records, sampling) {
  d <- dplyr::left_join(records, sampling, by = "year")
  out <- tibble::tibble(
    "Año" = as.character(d$year),
    Registros = fmt_int(d$n_records),
    "Primer registro" = fmt_time(d$first_record),
    "Último registro" = fmt_time(d$last_record),
    "Cobertura (%)" = fmt_num(d$coverage_pct),
    Huecos = fmt_int(d$n_gaps),
    "Horas en huecos" = fmt_int(d$gap_hours)
  )
  style_report_table(out, caption = "Registros y cobertura por fichero anual.",
    note = paste(
      "Cobertura: registros respecto a uno por minuto entre el primer y el",
      "último registro del fichero. Hueco: intervalo de más de 3 min entre",
      "registros consecutivos; los que cruzan el cambio de año se asignan al",
      "año del registro posterior."),
    align = c("l", "r", "l", "l", "r", "r", "r"))
}

#' Tabla-resumen de calidad de datos
table_quality_overview <- function(overview) {
  names(overview) <- c("Comprobación", "Resultado", "Tratamiento")
  style_report_table(overview,
    caption = "Resumen de las comprobaciones de calidad de datos.",
    note = paste("Ninguna anomalía se elimina salvo los duplicados exactos",
                 "(T2). % del periodo: porcentaje del intervalo calendario",
                 "entre el primer y el último registro."),
    align = c("l", "l", "l"),
    widths = c(`1` = "6.6cm", `2` = "4.9cm", `3` = "3.3cm"))
}

#' Versión REDUCIDA de la tabla de candidatas para el informe
#'
#' @param names Nombres limpios a mostrar (report_candidate_names()).
table_candidate_variables_short <- function(candidates,
                                            names = report_candidate_names()) {
  d <- candidates[match(names, candidates$name), ]
  missing_note <- if (all(d$pct_missing == 0, na.rm = TRUE)) {
    "Ninguna de estas variables tiene valores perdidos."
  } else {
    paste0("Valores perdidos (%): ",
           paste0(d$variable, " ", fmt_num(d$pct_missing, 2), collapse = "; "),
           ".")
  }
  out <- tibble::tibble(
    Columna = d$variable,
    Unidad = d$unit,
    "Spearman con Power out" = fmt_num(d$spearman_with_response, 2),
    "Papel preliminar" = d$role,
    Observaciones = d$note
  )
  style_report_table(out,
    caption = "Variables más relevantes para la modelización.",
    note = paste(
      "Propuesta para discusión, no una selección de variables.",
      "Spearman: correlación de rangos con la potencia de salida.",
      missing_note,
      "Tabla completa en outputs/tables/eda_candidate_variables.csv."),
    align = c("l", "l", "r", "l", "l"),
    widths = c(`3` = "1.7cm", `4` = "3.4cm", `5` = "5.6cm"))
}

#' Estadísticos descriptivos de variables continuas
#'
#' Decimales: media y DT con 1; mínimo, cuartiles y máximo con la precisión
#' de la variable (0 si es entera, 1 si no); % de ceros con 1.
table_descriptive <- function(summary_tbl) {
  q <- function(x) mapply(fmt_num, x, summary_tbl$digits)
  out <- tibble::tibble(
    Variable = vapply(summary_tbl$variable, var_label, character(1)),
    Media = fmt_num(summary_tbl$mean),
    DT = fmt_num(summary_tbl$sd),
    "Mín." = q(summary_tbl$min),
    Q1 = q(summary_tbl$q1),
    Mediana = q(summary_tbl$median),
    Q3 = q(summary_tbl$q3),
    "Máx." = q(summary_tbl$max),
    "% ceros" = fmt_num(summary_tbl$pct_zero)
  )
  style_report_table(out,
    caption = "Estadísticos descriptivos de las principales variables continuas.",
    note = "Calculados sobre todos los registros, incluidos los de potencia nula.")
}

#' Comprobaciones específicas de la variable respuesta (versión del informe)
#'
#' El detalle completo (meses, estados, episodios, perfiles, casos extremos)
#' está en outputs/tables/response_*.csv.
#'
#' @param cmp Salida de compare_power_measures().
#' @param high Salida de high_power_check().
table_response_checks <- function(cmp, high) {
  c1 <- cmp$summary
  h <- high$summary
  out <- tibble::tibble(
    Comprobación = c(
      "Correlación de Pearson / de Spearman entre Power out y Power reg",
      "Diferencia Power out - Power reg (W): mediana [P1; P99]",
      "Diferencia Power out - Power reg (W): mínima / máxima",
      paste0("Registros con Power out > ", fmt_int(h$threshold_w),
             " W (% de los registros con P > 0)"),
      "Episodios consecutivos por encima de ese umbral (máx. de registros)",
      "Máximo de Power out (W) y fecha",
      "Registros con Power out mayor que Power max registrado"),
    Resultado = c(
      paste0(fmt_num(c1$correlation, 4), " / ", fmt_num(c1$spearman, 2)),
      paste0(fmt_num(c1$median_diff, 0), " [", fmt_num(c1$p01_diff, 0), "; ",
             fmt_num(c1$p99_diff, 0), "]"),
      paste0(fmt_num(c1$min_diff, 0), " / ", fmt_num(c1$max_diff, 0)),
      paste0(fmt_int(h$n_above), " (", fmt_num(h$pct_of_producing, 2), " %)"),
      paste0(fmt_int(h$n_episodes), " (", fmt_int(h$max_episode_records), ")"),
      paste0(fmt_int(h$max_power), " (", h$time_of_max, ")"),
      fmt_int(h$n_above_power_max))
  )
  style_report_table(out, caption = "Comprobaciones de la variable respuesta.",
    note = paste0(
      "Ambas correlaciones se calculan sobre las mismas ", fmt_int(c1$n),
      " observaciones. El umbral de ", fmt_int(h$threshold_w),
      " W es la potencia nominal indicada por los autores del conjunto. ",
      "Ningún registro se elimina. Detalle por meses, estados y episodios ",
      "en outputs/tables/response_*.csv."),
    align = c("l", "r"), widths = c(`1` = "10cm"))
}

#' Distribución de los códigos de estado y potencia asociada
table_status_distribution <- function(status_tbl,
                                      status_label = "Turbine status",
                                      top = 6) {
  status_tbl <- utils::head(status_tbl, top)
  out <- tibble::tibble(
    "Código" = as.character(status_tbl$code),
    Registros = fmt_int(status_tbl$n),
    "% registros" = fmt_num(status_tbl$pct),
    "% con P > 0" = fmt_num(status_tbl$pct_producing),
    "Media (W)" = fmt_num(status_tbl$mean_power),
    "Mediana (W)" = fmt_num(status_tbl$median_power, 0),
    "Máx. (W)" = fmt_int(status_tbl$max_power)
  )
  style_report_table(out,
    caption = paste0("Potencia de salida según el código de ", status_label,
                     " (", top, " códigos más frecuentes)."),
    note = "El diccionario de datos no documenta el significado de los códigos.")
}

# -----------------------------------------------------------------------------
# Tablas auxiliares (no incluidas en el informe; reutilizables)
# -----------------------------------------------------------------------------

#' Tabla COMPLETA de variable respuesta y candidatas
table_candidate_variables <- function(candidates, roles_to_show = NULL) {
  if (!is.null(roles_to_show)) {
    candidates <- candidates[candidates$role %in% roles_to_show, ]
  }
  out <- tibble::tibble(
    Columna = candidates$variable,
    Unidad = candidates$unit,
    "% NA" = fmt_num(candidates$pct_missing),
    "Spearman" = fmt_num(candidates$spearman_with_response, 2),
    "Papel preliminar" = candidates$role,
    Observaciones = candidates$note
  )
  style_report_table(out, caption = "Variables candidatas (versión completa).",
                     widths = c(`5` = "3.2cm", `6` = "5.6cm"))
}

#' Resumen anual de la potencia
table_annual_power <- function(annual) {
  out <- tibble::tibble(
    "Año" = as.character(annual$year),
    Registros = fmt_int(annual$n_records),
    "% con P > 0" = fmt_num(annual$pct_producing),
    "Media (W)" = fmt_num(annual$mean_power),
    "Media si P > 0 (W)" = fmt_num(annual$mean_power_if_producing),
    "P99 (W)" = fmt_int(annual$p99_power),
    "Máx. (W)" = fmt_int(annual$max_power),
    "Energía neta (kWh)" = fmt_num(annual$net_energy_kwh)
  )
  style_report_table(out, caption = "Potencia de salida por año.",
    note = paste("Energía neta: suma de los incrementos del contador",
                 "acumulado dentro de tramos sin huecos."))
}

#' Valores perdidos (solo variables con alguno)
table_missing <- function(missing) {
  missing <- missing[missing$n_missing > 0, ]
  if (nrow(missing) == 0) return(invisible(NULL))
  out <- tibble::tibble(Variable = missing$variable,
                        "Perdidos" = fmt_int(missing$n_missing),
                        "%" = fmt_num(missing$pct_missing, 2))
  style_report_table(out, caption = "Variables con valores perdidos.")
}
