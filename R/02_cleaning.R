# =============================================================================
# 02_cleaning.R — Limpieza conservadora
# -----------------------------------------------------------------------------
# Responsable: María
# Este fichero separa DOS tipos de operaciones, que nunca se mezclan:
#
#   A) CORRECCIONES TÉCNICAS (clean_technical): no cambian la población
#      estudiada. Tipos, timestamps, duplicados exactos, orden temporal.
#      -> resultado: data/processed/swt_scada_clean.rds
#
#   B) FILTROS METODOLÓGICOS (build_analysis_dataset): cambian la población
#      (periodo, estados operativos, agregación). Requieren decisión del
#      grupo, registrada en docs/decisions.md. Por defecto NO filtran nada.
#      -> resultado: data/final/swt_analysis_dataset.rds
#
# Cada paso devuelve el número de filas antes/después para que toda
# exclusión quede trazada (atributo "cleaning_log").
# =============================================================================

#' Fila del registro de limpieza
log_entry <- function(step, kind, n_before, n_after, detail = "") {
  tibble::tibble(step = step, kind = kind, n_before = n_before,
                 n_after = n_after, n_removed = n_before - n_after,
                 detail = detail)
}

# -----------------------------------------------------------------------------
# A) Correcciones técnicas
# -----------------------------------------------------------------------------

#' Parsea `Log Time` (hora local de reloj) e `Inv Time` (GMT)
#'
#' * `Log Time` usa el formato YYYY:MM:DD:HH:MM:SS.SSS, pero en los datos el
#'   separador de milisegundos aparece unas veces como "." y otras como ","
#'   (observado en 2017). Se normaliza a "." antes de parsear.
#' * El diccionario indica que es hora LOCAL sin especificar zona horaria.
#'   Para no suponer reglas de horario de verano, se almacena como "hora de
#'   reloj" con etiqueta UTC: `log_time` NO debe convertirse a otra zona.
#' * `Inv Time` se interpreta como segundos Unix (GMT), según el diccionario.
#'
#' @return data con log_time, inv_time_utc y log_minus_inv_hours añadidas.
parse_datetime_columns <- function(data) {
  assert_columns(data, c("log_time_raw", "inv_time_raw"),
                 "parse_datetime_columns")
  normalized <- sub(",", ".", data$log_time_raw, fixed = TRUE)
  log_time <- as.POSIXct(normalized, format = "%Y:%m:%d:%H:%M:%OS",
                         tz = "UTC")
  inv_time <- as.POSIXct(suppressWarnings(as.numeric(data$inv_time_raw)),
                         origin = "1970-01-01", tz = "UTC")
  dplyr::mutate(
    data,
    log_time = log_time,
    inv_time_utc = inv_time,
    log_minus_inv_hours = as.numeric(difftime(log_time, inv_time,
                                              units = "hours")),
    .after = "source_file"
  )
}

#' Elimina filas duplicadas EXACTAS (todas las columnas originales iguales)
#'
#' Las filas con la misma marca temporal pero valores distintos NO se
#' eliminan: se señalan en la validación (03_validation.R).
remove_exact_duplicates <- function(data, schema = scada_schema()) {
  key_cols <- intersect(schema$name, names(data))
  # vctrs compara filas sin convertirlas a texto (duplicated() sobre un data
  # frame agota la memoria con millones de filas)
  first <- vctrs::vec_duplicate_id(data[key_cols])  # índice de la 1.ª aparición
  data[first == seq_len(nrow(data)), , drop = FALSE]
}

#' Ordena por tiempo de registro (orden estable)
sort_by_time <- function(data) {
  ordered <- order(data$log_time, method = "radix", na.last = TRUE)
  attr_value <- !identical(ordered, seq_len(nrow(data)))
  data <- data[ordered, , drop = FALSE]
  attr(data, "was_reordered") <- attr_value
  data
}

#' Aplica TODAS las correcciones técnicas y registra cada paso
#'
#' @param data Salida de load_all_years().
#' @return tibble limpio con atributo "cleaning_log".
clean_technical <- function(data) {
  parse_problems <- attr(data, "parse_problems")
  n0 <- nrow(data)
  data <- parse_datetime_columns(data)
  n_bad_time <- sum(is.na(data$log_time))
  time_order <- original_time_order(data)
  log1 <- log_entry("T1 Parseo de Log Time / Inv Time", "técnica", n0, n0,
                    paste0("Separador de ms ',' normalizado a '.'; ",
                           n_bad_time, " marcas no parseables (se conservan ",
                           "como NA y se señalan)"))

  n1 <- nrow(data)
  data <- remove_exact_duplicates(data)
  log2 <- log_entry("T2 Duplicados exactos", "técnica", n1, nrow(data),
                    "Filas idénticas en todas las columnas originales")

  n2 <- nrow(data)
  data <- sort_by_time(data)
  log3 <- log_entry("T3 Orden temporal", "técnica", n2, nrow(data),
                    if (isTRUE(attr(data, "was_reordered")))
                      "Se reordenaron filas por log_time"
                    else "Las filas ya estaban en orden temporal")

  # T5: las columnas de texto originales ya están parseadas en log_time e
  # inv_time_utc (con milisegundos); se eliminan para ahorrar memoria.
  data$log_time_raw <- NULL
  data$inv_time_raw <- NULL
  log4 <- log_entry("T5 Columnas de texto de tiempo", "técnica", nrow(data),
                    nrow(data), "log_time_raw e inv_time_raw sustituidas por log_time e inv_time_utc")
  attr(data, "cleaning_log") <- dplyr::bind_rows(log1, log2, log3, log4)
  attr(data, "parse_problems") <- parse_problems
  attr(data, "time_order") <- time_order
  data
}

#' Retrocesos de log_time en el orden original de cada fichero
#'
#' Un retroceso (registro con hora anterior al previo) podría reflejar, por
#' ejemplo, un ajuste del reloj; solo se cuenta, no se interpreta.
original_time_order <- function(data) {
  t <- as.numeric(data$log_time)
  pieces <- split(t, data$source_file)
  tibble::tibble(
    file = names(pieces),
    n_backward = vapply(pieces, function(x) sum(diff(x) < 0, na.rm = TRUE),
                        numeric(1)),
    max_backward_s = vapply(pieces, function(x) {
      d <- diff(x)
      if (any(d < 0, na.rm = TRUE)) -min(d, na.rm = TRUE) else 0
    }, numeric(1))
  )
}

# -----------------------------------------------------------------------------
# B) Filtros metodológicos (decisiones del grupo; por defecto, sin efecto)
# -----------------------------------------------------------------------------

#' D-M2: restringe el periodo de estudio (NULL = sin restricción)
filter_period <- function(data, start = NULL, end = NULL) {
  keep <- rep(TRUE, nrow(data))
  if (!is.null(start)) keep <- keep & data$log_time >= as.POSIXct(start, tz = "UTC")
  if (!is.null(end)) keep <- keep & data$log_time < as.POSIXct(end, tz = "UTC") + 86400
  data[keep & !is.na(data$log_time), , drop = FALSE]
}

#' D-M3: conserva solo ciertos códigos de `turbine_status` (NULL = todos)
#'
#' ATENCIÓN: el diccionario NO documenta el significado de los códigos.
#' Usar este filtro exige justificación externa (p. ej. manual del fabricante).
filter_turbine_status <- function(data, keep_codes = NULL) {
  if (is.null(keep_codes)) return(data)
  data[data$turbine_status %in% keep_codes, , drop = FALSE]
}

#' D-M4: agrega a intervalos regulares ("none", "10min", "hour")
#'
#' Variables continuas: media. Contadores acumulados: último valor.
#' Códigos de estado: valor más frecuente en el intervalo.
#' n_records: nº de registros que contribuyen a cada intervalo.
aggregate_time <- function(data, interval = c("none", "10min", "hour"),
                           schema = scada_schema()) {
  interval <- match.arg(interval)
  if (interval == "none") return(data)
  width <- c("10min" = 600, hour = 3600)[[interval]]
  data$interval_start <- as.POSIXct(floor(as.numeric(data$log_time) / width) *
                                      width, origin = "1970-01-01", tz = "UTC")
  continuous <- intersect(schema$name[schema$type %in%
                                        c("continua", "entera",
                                          "entera (derivada de RPM)")],
                          names(data))
  counters <- intersect(schema$name[schema$type == "contador acumulado"],
                        names(data))
  codes <- intersect(status_columns(), names(data))
  mode_value <- function(x) {
    x <- x[!is.na(x)]
    if (length(x) == 0) return(NA_real_)
    as.numeric(names(which.max(table(x))))
  }
  dplyr::summarise(
    dplyr::group_by(data, interval_start),
    n_records = dplyr::n(),
    dplyr::across(dplyr::all_of(continuous), ~ mean(.x, na.rm = TRUE)),
    dplyr::across(dplyr::all_of(counters), ~ dplyr::last(.x)),
    dplyr::across(dplyr::all_of(codes), mode_value),
    .groups = "drop"
  ) |>
    dplyr::rename(log_time = interval_start)
}

#' Construye el conjunto de análisis aplicando los filtros metodológicos
#'
#' Todos los argumentos son explícitos; sus valores deben salir de
#' config.yml (sección analysis) y estar justificados en docs/decisions.md.
#'
#' @return tibble con atributo "cleaning_log" (filtros aplicados).
build_analysis_dataset <- function(data, period_start = NULL,
                                   period_end = NULL,
                                   turbine_status_keep = NULL,
                                   aggregation = "none") {
  steps <- list()
  n0 <- nrow(data)
  data <- filter_period(data, period_start, period_end)
  steps[[1]] <- log_entry("D-M2 Periodo de estudio", "metodológica", n0,
                          nrow(data),
                          paste0("inicio=", period_start %||% "sin límite",
                                 "; fin=", period_end %||% "sin límite"))
  n1 <- nrow(data)
  data <- filter_turbine_status(data, turbine_status_keep)
  steps[[2]] <- log_entry("D-M3 Estados de turbina", "metodológica", n1,
                          nrow(data),
                          if (is.null(turbine_status_keep)) "sin filtro"
                          else paste("conservar:",
                                     paste(turbine_status_keep, collapse = ", ")))
  n2 <- nrow(data)
  data <- aggregate_time(data, aggregation)
  steps[[3]] <- log_entry("D-M4 Agregación temporal", "metodológica", n2,
                          nrow(data), paste("intervalo:", aggregation))
  attr(data, "cleaning_log") <- dplyr::bind_rows(steps)
  data
}

#' Operador "valor por defecto si NULL"
`%||%` <- function(x, y) if (is.null(x)) y else x

# -----------------------------------------------------------------------------
# C) Pipeline completo (lo ejecuta scripts/01_build_dataset.R)
# -----------------------------------------------------------------------------

#' Construye data/processed y data/final y el informe de calidad
#'
#' 1. Detecta ficheros y guarda su huella MD5.
#' 2. Lee y combina años (validando el esquema).
#' 3. Aplica SOLO correcciones técnicas -> processed.
#' 4. Ejecuta las comprobaciones de calidad -> outputs/tables/quality_*.csv
#' 5. Aplica los filtros metodológicos definidos en config.yml -> final.
#' 6. Verifica que data/raw no ha cambiado.
#'
#' @return lista invisible con las comprobaciones de calidad y nº de filas
#'   (los conjuntos se leen con load_analysis_data()).
build_datasets <- function(config = load_config()) {
  gap <- config$validation$gap_threshold_seconds
  response <- config$analysis$response

  log_step("Detectando ficheros originales")
  files <- list_raw_files()
  manifest_before <- raw_manifest(files)

  log_step("Leyendo ", nrow(files), " fichero(s): ",
           paste(files$year, collapse = ", "))
  raw <- load_all_years(files)

  log_step("Correcciones técnicas")
  clean <- clean_technical(raw)
  rm(raw)

  log_step("Comprobaciones de calidad")
  checks <- run_data_quality_checks(clean, files, config)
  write_quality_report(checks)

  log_step("Variables derivadas y guardado de data/processed")
  processed <- add_features(clean, gap, response)
  n_processed <- nrow(processed)
  # Se conservan los registros de limpieza para que el informe los recalcule
  attr(processed, "cleaning_log") <- attr(clean, "cleaning_log")
  attr(processed, "parse_problems") <- attr(clean, "parse_problems")
  attr(processed, "time_order") <- attr(clean, "time_order")
  ensure_dir(project_path("processed_dir"))
  saveRDS(processed, project_path("processed_dir",
                                  config$outputs$processed_file))
  rm(processed); invisible(gc())

  log_step("Filtros metodológicos (config.yml -> analysis)")
  a <- config$analysis
  final <- build_analysis_dataset(clean, a$period_start, a$period_end,
                                  a$turbine_status_keep, a$aggregation)
  method_log <- attr(final, "cleaning_log")
  final <- add_features(final, gap, response)
  n_final <- nrow(final)
  ensure_dir(project_path("final_dir"))
  saveRDS(final, project_path("final_dir", config$outputs$final_file))
  rm(final); invisible(gc())
  save_table(dplyr::bind_rows(attr(clean, "cleaning_log"), method_log),
             "cleaning_log")

  manifest_after <- raw_manifest(files)
  if (!identical(manifest_before$md5, manifest_after$md5)) {
    stop("¡Los ficheros de data/raw han cambiado durante el proceso!",
         call. = FALSE)
  }
  log_step("Hecho: processed = ", n_processed, " filas; final = ",
           n_final, " filas")
  invisible(list(checks = checks, n_processed = n_processed,
                 n_final = n_final))
}

#' Carga el conjunto procesado o final (lo construye si no existe)
#'
#' @param which "processed" (solo correcciones técnicas; usado en el EDA) o
#'   "final" (tras filtros metodológicos; usado en el modelo).
load_analysis_data <- function(which = c("processed", "final"),
                               config = load_config(),
                               build_if_missing = TRUE) {
  which <- match.arg(which)
  path <- if (which == "processed") {
    project_path("processed_dir", config$outputs$processed_file)
  } else {
    project_path("final_dir", config$outputs$final_file)
  }
  if (!file.exists(path)) {
    if (!build_if_missing) {
      stop("No existe ", path, ". Ejecuta scripts/01_build_dataset.R",
           call. = FALSE)
    }
    message("No existe ", basename(path), "; se construye ahora.")
    build_datasets(config)
  }
  readRDS(path)
}
