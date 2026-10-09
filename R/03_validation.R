# =============================================================================
# 03_validation.R — Validación sistemática y calidad de datos
# -----------------------------------------------------------------------------
# Responsable: María
# Principio: IDENTIFICAR y DOCUMENTAR, nunca eliminar. Ninguna función de este
# fichero modifica los datos; todas devuelven tablas de diagnóstico.
# Solo se comprueban "valores imposibles" cuando el diccionario los respalda
# (constantes documentadas, contadores acumulados, valores enteros).
# =============================================================================

#' Columnas numéricas del esquema presentes en los datos
numeric_schema_columns <- function(data, schema = scada_schema()) {
  cols <- schema$name[!schema$type %in% "tiempo"]
  intersect(cols, names(data))
}

#' Registros, rango de fechas y cobertura por año
#'
#' coverage_pct = registros / minutos entre el primer y último registro
#' (el diccionario indica 1 registro por minuto).
summarise_records_by_year <- function(data) {
  dplyr::group_by(data, year = file_year) |>
    dplyr::summarise(
      n_records = dplyr::n(),
      first_record = min(log_time, na.rm = TRUE),
      last_record = max(log_time, na.rm = TRUE),
      n_days_with_data = dplyr::n_distinct(as.Date(log_time)),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      span_days = round(as.numeric(difftime(last_record, first_record,
                                            units = "days")), 1),
      coverage_pct = 100 * n_records /
        (as.numeric(difftime(last_record, first_record, units = "mins")) + 1)
    )
}

#' Intervalos entre registros consecutivos (segundos)
sampling_intervals <- function(data) {
  c(NA_real_, diff(as.numeric(data$log_time)))
}

#' Resumen de la frecuencia de muestreo por año
summarise_sampling_interval <- function(data, gap_threshold = 180) {
  data$dt_s <- sampling_intervals(data)
  dplyr::group_by(data, year = file_year) |>
    dplyr::summarise(
      median_s = stats::median(dt_s, na.rm = TRUE),
      p05_s = stats::quantile(dt_s, 0.05, na.rm = TRUE),
      p95_s = stats::quantile(dt_s, 0.95, na.rm = TRUE),
      pct_50_70s = 100 * mean(dt_s >= 50 & dt_s <= 70, na.rm = TRUE),
      n_non_positive = sum(dt_s <= 0, na.rm = TRUE),
      n_gaps = sum(dt_s > gap_threshold, na.rm = TRUE),
      gap_hours = sum(dt_s[dt_s > gap_threshold], na.rm = TRUE) / 3600,
      .groups = "drop"
    )
}

#' Lista de huecos temporales mayores que el umbral
#'
#' DEFINICIÓN DE HUECO: intervalo entre dos registros consecutivos (todos los
#' años combinados y ordenados por log_time) mayor que `gap_threshold`
#' segundos. Su duración es el intervalo completo: desde el último registro
#' antes del hueco hasta el primero después (incluye el minuto nominal de
#' muestreo). Un hueco que cruza el cambio de año se cuenta una sola vez y se
#' asigna al año del fichero del registro posterior (`file_year`).
detect_time_gaps <- function(data, gap_threshold = 180) {
  dt <- sampling_intervals(data)
  idx <- which(dt > gap_threshold)
  file_year <- if ("file_year" %in% names(data)) data$file_year else NA_integer_
  tibble::tibble(
    gap_start = data$log_time[idx - 1],
    gap_end = data$log_time[idx],
    duration_hours = dt[idx] / 3600,
    file_year_after = rep_len(file_year, nrow(data))[idx],
    crosses_year = format(data$log_time[idx - 1], "%Y") !=
      format(data$log_time[idx], "%Y")
  ) |>
    dplyr::arrange(dplyr::desc(duration_hours))
}

#' Verificación contable de los huecos
#'
#' Comprueba que el periodo total (primer a último registro) se reparte
#' exactamente entre intervalos normales (<= umbral) y huecos (> umbral), y
#' ofrece la duración "neta" sin observaciones (descontando el intervalo
#' nominal de `nominal_s` segundos de cada hueco).
#'
#' @return tibble de una fila; `identity_error_s` debe ser 0.
gap_accounting <- function(data, gap_threshold = 180, nominal_s = 60) {
  dt <- sampling_intervals(data)[-1]
  is_gap <- dt > gap_threshold
  span_s <- diff(as.numeric(range(data$log_time, na.rm = TRUE)))
  cross <- format(data$log_time[-nrow(data)], "%Y") !=
    format(data$log_time[-1], "%Y")
  tibble::tibble(
    span_hours = span_s / 3600,
    n_records = nrow(data),
    n_intervals = length(dt),
    n_gaps = sum(is_gap),
    gap_hours = sum(dt[is_gap]) / 3600,
    gap_hours_net = sum(dt[is_gap] - nominal_s) / 3600,
    observed_hours = sum(dt[!is_gap]) / 3600,
    identity_error_s = span_s - sum(dt),
    pct_span_in_gaps = 100 * sum(dt[is_gap]) / span_s,
    n_gaps_crossing_year = sum(is_gap & cross),
    hours_gaps_crossing_year = sum(dt[is_gap & cross]) / 3600,
    longest_gap_hours = if (any(is_gap)) max(dt[is_gap]) / 3600 else 0,
    coverage_pct = 100 * nrow(data) / (span_s / 60 + 1)
  )
}

#' Marcas temporales repetidas con valores distintos (no son duplicados exactos)
detect_duplicated_timestamps <- function(data) {
  dup <- duplicated(data$log_time) | duplicated(data$log_time, fromLast = TRUE)
  tibble::tibble(
    n_rows_sharing_timestamp = sum(dup & !is.na(data$log_time)),
    n_distinct_timestamps_repeated = dplyr::n_distinct(data$log_time[dup])
  )
}

#' Valores perdidos por variable
summarise_missing <- function(data, vars = numeric_schema_columns(data)) {
  tibble::tibble(
    variable = vars,
    n_missing = vapply(vars, function(v) sum(is.na(data[[v]])), numeric(1)),
    pct_missing = 100 * n_missing / nrow(data)
  )
}

#' Rangos, tipos y variables (casi) constantes
#'
#' @param near_constant_threshold Proporción del valor modal a partir de la
#'   cual una variable se marca como casi constante.
summarise_variable_ranges <- function(data, schema = scada_schema(),
                                      near_constant_threshold = 0.99) {
  vars <- numeric_schema_columns(data, schema)
  rows <- lapply(vars, function(v) {
    x <- data[[v]]
    x_ok <- x[!is.na(x)]
    # frecuencia del valor modal sin table() (evita convertir a texto)
    top_share <- if (length(x_ok)) {
      max(tabulate(match(x_ok, unique(x_ok)))) / length(x_ok)
    } else NA
    q <- if (length(x_ok)) stats::quantile(x_ok, c(0, .01, .5, .99, 1),
                                           names = FALSE) else rep(NA, 5)
    tibble::tibble(
      variable = v,
      r_class = class(x)[1],
      all_integer = length(x_ok) > 0 && all(x_ok == round(x_ok)),
      min = q[1], p01 = q[2], median = q[3], p99 = q[4], max = q[5],
      n_distinct = length(unique(x_ok)),
      top_value_share = top_share,
      constant = length(unique(x_ok)) <= 1,
      near_constant = !is.na(top_share) && top_share >= near_constant_threshold
    )
  })
  dplyr::bind_rows(rows) |>
    dplyr::left_join(schema[c("name", "type", "unit")],
                     by = c("variable" = "name"))
}

#' Comprueba las constantes que documenta el diccionario (SN#, versiones)
check_documented_constants <- function(data,
                                       expected = documented_constants()) {
  tibble::tibble(
    variable = names(expected),
    documented_value = unname(expected),
    observed_values = vapply(names(expected), function(v)
      paste(sort(unique(data[[v]])), collapse = ", "), character(1)),
    n_different = vapply(names(expected), function(v)
      sum(data[[v]] != expected[[v]], na.rm = TRUE), numeric(1))
  )
}

#' Contadores acumulados: nº de descensos (posibles reinicios) por fichero
#'
#' El diccionario define "watt-hours" y "Event count" como acumulados, por lo
#' que un descenso es incoherente con la definición. Solo se cuenta; no se
#' corrige.
check_cumulative_counters <- function(data,
                                      counters = c("energy_wh_cum",
                                                   "event_count_cum")) {
  dplyr::group_by(data, year = file_year) |>
    dplyr::summarise(
      dplyr::across(dplyr::all_of(counters),
                    ~ sum(diff(.x) < 0, na.rm = TRUE),
                    .names = "n_decreases_{.col}"),
      .groups = "drop"
    )
}

#' Describe los descensos de un contador acumulado
#'
#' Para cada descenso: tamaño, si ocurre tras un hueco y si la potencia es
#' nula en el registro posterior. Solo describe; no corrige.
describe_counter_decreases <- function(data, counter = "energy_wh_cum",
                                       response = "power_out_w",
                                       gap_threshold = 180) {
  x <- data[[counter]]
  step <- c(NA_real_, diff(x))
  after_gap <- sampling_intervals(data) > gap_threshold
  dec <- which(step < 0)
  size <- -step[dec]
  tibble::tibble(
    counter = counter,
    n_decreases = length(dec),
    pct_size_1 = if (length(dec)) 100 * mean(size == 1) else NA_real_,
    median_size = if (length(dec)) stats::median(size) else NA_real_,
    max_size = if (length(dec)) max(size) else NA_real_,
    n_after_gap = sum(after_gap[dec], na.rm = TRUE),
    pct_response_zero = if (length(dec)) {
      100 * mean(data[[response]][dec] == 0, na.rm = TRUE)
    } else NA_real_
  )
}

#' Inventario de códigos de estado: frecuencia y porcentaje por variable
status_code_inventory <- function(data, vars = status_columns()) {
  rows <- lapply(vars, function(v) {
    tab <- table(data[[v]], useNA = "ifany")
    tibble::tibble(variable = v, code = names(tab), n = as.numeric(tab),
                   pct = 100 * as.numeric(tab) / nrow(data))
  })
  dplyr::bind_rows(rows) |>
    dplyr::arrange(variable, dplyr::desc(n))
}

#' Coherencia entre años de magnitudes que deberían ser estables
cross_year_consistency <- function(data) {
  dplyr::group_by(data, year = file_year) |>
    dplyr::summarise(
      serial_numbers = paste(unique(serial_number), collapse = ","),
      firmware = paste(unique(software_rev), collapse = ","),
      op_version = paste(unique(op_version), collapse = ","),
      power_max_values = paste(sort(unique(power_max_w)), collapse = ","),
      median_log_minus_inv_h = stats::median(log_minus_inv_hours,
                                             na.rm = TRUE),
      n_distinct_turbine_status = dplyr::n_distinct(turbine_status),
      pct_power_zero = 100 * mean(power_out_w == 0, na.rm = TRUE),
      n_records_other_year = sum(format(log_time, "%Y") !=
                                   as.character(dplyr::first(file_year)),
                                 na.rm = TRUE),
      .groups = "drop"
    )
}

#' Ejecuta todas las comprobaciones de calidad
#'
#' @param data Datos tras clean_technical() (timestamps parseados).
#' @param files Salida de list_raw_files().
#' @param config Configuración (load_config()).
#' @return lista nombrada de tablas.
run_data_quality_checks <- function(data, files, config = load_config()) {
  thr <- config$validation$gap_threshold_seconds
  list(
    raw_manifest = raw_manifest(files),
    schema = validate_schema(files),
    parse_problems = attr(data, "parse_problems") %||% tibble::tibble(),
    cleaning_log = attr(data, "cleaning_log"),
    records_by_year = summarise_records_by_year(data),
    sampling = summarise_sampling_interval(data, thr),
    gaps = detect_time_gaps(data, thr),
    gap_accounting = gap_accounting(data, thr),
    time_order = attr(data, "time_order") %||% tibble::tibble(),
    duplicated_timestamps = detect_duplicated_timestamps(data),
    unparsed_timestamps = tibble::tibble(n = sum(is.na(data$log_time))),
    missing = summarise_missing(data),
    ranges = summarise_variable_ranges(
      data, near_constant_threshold = config$validation$near_constant_threshold),
    documented_constants = check_documented_constants(data),
    counters = check_cumulative_counters(data),
    counter_decreases = dplyr::bind_rows(
      describe_counter_decreases(data, "energy_wh_cum", gap_threshold = thr),
      describe_counter_decreases(data, "event_count_cum", gap_threshold = thr)),
    status_codes = status_code_inventory(data),
    cross_year = cross_year_consistency(data)
  )
}

#' Tabla-resumen de la calidad de datos (una fila por comprobación)
quality_overview <- function(checks) {
  r <- checks$ranges
  ga <- checks$gap_accounting
  cd <- checks$counter_decreases
  backward <- if (nrow(checks$time_order)) sum(checks$time_order$n_backward) else NA
  tibble::tibble(
    check = c("Ficheros con esquema compatible",
              "Variables con NA / valores no numéricos / fechas no parseables",
              "Duplicados exactos / marcas repetidas con otros valores",
              "Retrocesos de Log Time en el orden original",
              "Registros fuera del año de su fichero",
              "Huecos > 3 min (nº; horas; % del periodo)",
              "Variables constantes",
              "Variables casi constantes (valor modal >= 99 %)",
              "Constantes documentadas con valores distintos",
              "Descensos del contador de energía"),
    result = c(
      paste0(sum(checks$schema$compatible), " de ", nrow(checks$schema)),
      paste0(fmt_int(sum(checks$missing$n_missing > 0)), " / ",
             fmt_int(nrow(checks$parse_problems)), " / ",
             fmt_int(checks$unparsed_timestamps$n)),
      paste0(fmt_int(sum(checks$cleaning_log$n_removed[
        grepl("^T2", checks$cleaning_log$step)])), " / ",
        fmt_int(checks$duplicated_timestamps$n_rows_sharing_timestamp)),
      fmt_int(backward),
      fmt_int(sum(checks$cross_year$n_records_other_year)),
      paste0(fmt_int(ga$n_gaps), "; ", fmt_int(ga$gap_hours), " h; ",
             fmt_num(ga$pct_span_in_gaps), " %"),
      paste(r$variable[r$constant], collapse = ", "),
      paste(r$variable[r$near_constant & !r$constant], collapse = ", "),
      fmt_int(sum(checks$documented_constants$n_different)),
      paste0(fmt_int(cd$n_decreases[1]), " (", fmt_num(cd$pct_size_1[1]),
             " % de 1 Wh; máx. ", fmt_int(cd$max_size[1]), " Wh)")
    ),
    action = c("Detener si no", "Señalar", "Eliminar exactos (T2); señalar",
               "Documentar", "Documentar", "Documentar (dependencia temporal)",
               "No informativas", "Documentar", "Documentar",
               "Documentar; sin interpretar")
  )
}

#' Escribe todas las tablas de calidad en outputs/tables (CSV)
write_quality_report <- function(checks, dir = project_path("tables_dir")) {
  ensure_dir(dir)
  for (nm in names(checks)) {
    if (nrow(checks[[nm]]) > 0) {
      save_table(checks[[nm]], paste0("quality_", nm), dir)
    }
  }
  save_table(quality_overview(checks), "quality_overview", dir)
  invisible(dir)
}
