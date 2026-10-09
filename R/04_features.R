# =============================================================================
# 04_features.R — Variables derivadas
# -----------------------------------------------------------------------------
# Responsable: María
# Solo se crean variables DESCRIPTIVAS derivadas de forma determinista de las
# originales. Ninguna filtra filas. Que una variable derivada se use después
# como predictora es una decisión del modelo (Álvaro).
# =============================================================================

#' Componentes de calendario a partir de `log_time` (hora local de reloj)
add_time_features <- function(data) {
  assert_columns(data, "log_time", "add_time_features")
  dplyr::mutate(
    data,
    date = as.Date(log_time),
    year = as.integer(format(log_time, "%Y")),
    month = as.integer(format(log_time, "%m")),
    hour = as.integer(format(log_time, "%H"))
  )
}

#' Intervalo con el registro anterior, huecos y tramos continuos
#'
#' segment_id identifica tramos sin huecos > gap_threshold; útil para
#' estudiar la dependencia temporal sin unir observaciones separadas por
#' horas o días.
add_sampling_features <- function(data, gap_threshold = 180) {
  dt <- c(NA_real_, diff(as.numeric(data$log_time)))
  new_segment <- is.na(dt) | dt > gap_threshold
  dplyr::mutate(data,
                dt_prev_s = dt,
                starts_after_gap = new_segment,
                segment_id = cumsum(new_segment))
}

#' Energía producida desde el registro anterior (Wh), a partir del contador
#'
#' Se deja NA solo si hay un hueco entre registros (el incremento abarcaría
#' el hueco). Los incrementos NEGATIVOS se conservan: en 2017 el contador
#' desciende en pasos de 1 Wh en periodos sin producción, hecho que el
#' diccionario no explica (ver docs/methodology_notes.md). Ocultarlos
#' sesgaría cualquier resumen de energía.
add_energy_increment <- function(data) {
  assert_columns(data, c("energy_wh_cum", "starts_after_gap"),
                 "add_energy_increment")
  increment <- c(NA_real_, diff(data$energy_wh_cum))
  dplyr::mutate(data,
                energy_increment_wh = ifelse(data$starts_after_gap,
                                             NA_real_, increment))
}

#' Indicador descriptivo de producción (potencia de salida > 0)
#'
#' NO es un filtro ni un estado operativo documentado: solo describe si en
#' ese registro el inversor entrega potencia.
add_production_indicator <- function(data, response = "power_out_w") {
  data$producing <- data[[response]] > 0
  data
}

#' Aplica todas las variables derivadas
add_features <- function(data, gap_threshold = 180,
                         response = "power_out_w") {
  data |>
    add_time_features() |>
    add_sampling_features(gap_threshold) |>
    add_energy_increment() |>
    add_production_indicator(response)
}
