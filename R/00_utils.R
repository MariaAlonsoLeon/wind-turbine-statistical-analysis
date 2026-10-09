# =============================================================================
# 00_utils.R — Utilidades generales del proyecto
# -----------------------------------------------------------------------------
# Responsable: María
# Contenido: carga de paquetes y funciones, configuración, rutas, formato
# numérico en español y guardado de tablas/figuras.
# Ninguna función de este fichero toma decisiones metodológicas.
# =============================================================================

#' Paquetes que necesita el proyecto (única lista; renv.lock fija versiones)
project_packages <- function() {
  c("readr", "dplyr", "tibble", "rlang", "vctrs", "ggplot2", "scales",
    "knitr", "kableExtra", "rmarkdown", "bookdown", "here", "yaml")
}

#' Carga los paquetes del proyecto con un error informativo si falta alguno
load_project_packages <- function(packages = project_packages()) {
  missing <- packages[!vapply(packages, requireNamespace, logical(1),
                              quietly = TRUE)]
  if (length(missing) > 0) {
    stop("Faltan paquetes: ", paste(missing, collapse = ", "),
         ".\nEjecuta renv::restore() (o source('scripts/00_setup.R')).",
         call. = FALSE)
  }
  for (pkg in packages) {
    suppressPackageStartupMessages(library(pkg, character.only = TRUE))
  }
  invisible(packages)
}

#' Carga todas las funciones de R/ en orden numérico
#'
#' Se usa desde los scripts, el Rmd y los tests, para que todos ejecuten
#' exactamente el mismo código.
source_project_functions <- function(r_dir = here::here("R")) {
  files <- sort(list.files(r_dir, pattern = "\\.R$", full.names = TRUE))
  for (f in files) source(f, local = globalenv())
  invisible(basename(files))
}

#' Lee config/config.yml
load_config <- function(path = here::here("config", "config.yml")) {
  if (!file.exists(path)) {
    stop("No se encuentra el fichero de configuración: ", path, call. = FALSE)
  }
  # readLines(encoding = "UTF-8") marca el texto sin reconvertirlo, por lo
  # que funciona aunque la sesión no use un locale UTF-8.
  yaml::yaml.load(paste(readLines(path, encoding = "UTF-8", warn = FALSE),
                        collapse = "\n"))
}

#' Resuelve una ruta de la sección `paths` de la configuración
project_path <- function(key, ..., config = load_config()) {
  rel <- config$paths[[key]]
  if (is.null(rel)) stop("Ruta no definida en config.yml: ", key, call. = FALSE)
  here::here(rel, ...)
}

#' Crea un directorio si no existe (los directorios de salida no se versionan)
ensure_dir <- function(path) {
  if (!dir.exists(path)) dir.create(path, recursive = TRUE)
  invisible(path)
}

#' Mensaje de progreso uniforme para los scripts
log_step <- function(...) {
  message(format(Sys.time(), "[%H:%M:%S] "), ...)
}

# -----------------------------------------------------------------------------
# Formato en español
# -----------------------------------------------------------------------------

#' Formatea números con coma decimal y punto de miles
fmt_num <- function(x, digits = 1) {
  ifelse(is.na(x), "--",
         formatC(x, format = "f", digits = digits, big.mark = ".",
                 decimal.mark = ","))
}

#' Formatea enteros con punto de miles
fmt_int <- function(x) fmt_num(x, digits = 0)

#' Formatea un porcentaje (x ya en escala 0-100)
fmt_pct <- function(x, digits = 1) paste0(fmt_num(x, digits), " %")

#' Número + sustantivo con concordancia: n_es(1, "registro", "registros")
n_es <- function(n, singular, plural) {
  paste(fmt_int(n), if (isTRUE(n == 1)) singular else plural)
}

#' Fecha en español sin depender del locale del sistema
format_date_es <- function(date = Sys.Date()) {
  meses <- c("enero", "febrero", "marzo", "abril", "mayo", "junio", "julio",
             "agosto", "septiembre", "octubre", "noviembre", "diciembre")
  date <- as.Date(date)
  paste(as.integer(format(date, "%d")), "de",
        meses[as.integer(format(date, "%m"))], "de", format(date, "%Y"))
}

# -----------------------------------------------------------------------------
# Guardado de resultados
# -----------------------------------------------------------------------------

#' Guarda una tabla (data frame) como CSV en outputs/tables
save_table <- function(table, name, dir = project_path("tables_dir")) {
  ensure_dir(dir)
  path <- file.path(dir, paste0(name, ".csv"))
  readr::write_csv(table, path, na = "")
  invisible(path)
}

#' Guarda un gráfico ggplot como PDF y PNG en outputs/figures
save_figure <- function(plot, name, width = 6.5, height = 3.6,
                        dir = project_path("figures_dir")) {
  ensure_dir(dir)
  paths <- file.path(dir, paste0(name, c(".pdf", ".png")))
  ggplot2::ggsave(paths[1], plot, width = width, height = height)
  ggplot2::ggsave(paths[2], plot, width = width, height = height, dpi = 150)
  invisible(paths)
}

#' Comprueba que un data frame contiene las columnas indicadas
assert_columns <- function(data, columns, where = "") {
  missing <- setdiff(columns, names(data))
  if (length(missing) > 0) {
    stop(where, ": faltan columnas: ", paste(missing, collapse = ", "),
         call. = FALSE)
  }
  invisible(TRUE)
}
