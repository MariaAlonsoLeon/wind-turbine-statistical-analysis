# =============================================================================
# 99_render_report.R — Genera el informe PDF
# -----------------------------------------------------------------------------
# Uso:  source("scripts/99_render_report.R")
# Equivale al botón Knit / rmarkdown::render("reports/actividad1.Rmd"), en un
# entorno limpio, y deja el PDF en reports/output/.
# Si data/processed no existe, el propio Rmd lo construye.
#
# Nota: se renderiza en la carpeta del Rmd y DESPUÉS se mueve el PDF. Si se
# usa output_dir, knitr escribe rutas absolutas de figuras en el .tex y, en
# Windows, sus barras invertidas (C:\Users\...) rompen la compilación LaTeX.
# =============================================================================

source(here::here("R", "00_utils.R"))
out_dir <- project_path("report_output_dir")
ensure_dir(out_dir)

pdf <- rmarkdown::render(
  input = here::here("reports", "actividad1.Rmd"),
  envir = new.env(parent = globalenv()),
  quiet = TRUE
)
target <- file.path(out_dir, basename(pdf))
if (!file.copy(pdf, target, overwrite = TRUE)) {
  stop("No se pudo copiar el PDF a ", out_dir,
       " (¿está abierto en otro programa?)", call. = FALSE)
}
unlink(pdf)
message("Informe generado: ", target)
