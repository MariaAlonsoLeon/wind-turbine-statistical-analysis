# Carga las funciones del proyecto para los tests (mismo código que scripts y Rmd)
source(here::here("R", "00_utils.R"))
load_project_packages()
source_project_functions()

fixture_dir <- function(...) test_path("fixtures", ...)
fixture_pattern <- "^data_swt_iee_usp_(\\d{4})\\.csv$"
fixture_files <- function(dir = "valid") {
  list_raw_files(fixture_dir(dir), pattern = fixture_pattern)
}
