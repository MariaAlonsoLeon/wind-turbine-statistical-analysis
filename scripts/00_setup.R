# =============================================================================
# 00_setup.R — Comprobación del entorno
# -----------------------------------------------------------------------------
# Uso:  source("scripts/00_setup.R")
# Comprueba paquetes, configuración y presencia de los datos originales.
# No modifica nada salvo, si faltan paquetes, ofrecer renv::restore().
# =============================================================================

if (!requireNamespace("here", quietly = TRUE)) {
  stop("Instala 'here' o ejecuta renv::restore() antes de continuar.")
}
source(here::here("R", "00_utils.R"))

pkgs <- project_packages()
missing_pkgs <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_pkgs) > 0) {
  message("Faltan paquetes: ", paste(missing_pkgs, collapse = ", "))
  if (requireNamespace("renv", quietly = TRUE) &&
      file.exists(here::here("renv.lock"))) {
    message("Restaurando versiones fijadas en renv.lock ...")
    renv::restore(prompt = FALSE)
  } else {
    stop("Instala renv y ejecuta renv::restore().")
  }
}

load_project_packages()
source_project_functions()
config <- load_config()

files <- tryCatch(list_raw_files(), error = function(e) NULL)
expected <- config$raw_format$expected_years
if (is.null(files)) {
  message("AVISO: no hay datos en data/raw. Ver data/README.md.")
} else {
  message("Ficheros encontrados: ", paste(files$file, collapse = ", "))
  absent <- setdiff(expected, files$year)
  if (length(absent) > 0) {
    message("AVISO: faltan los años ", paste(absent, collapse = ", "),
            ". El proyecto funciona con los presentes, pero el análisis ",
            "final debe usar los seis ficheros.")
  }
}
message("Entorno listo (R ", getRversion(), ").")
