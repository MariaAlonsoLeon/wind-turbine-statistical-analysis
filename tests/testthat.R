# Ejecuta todos los tests:  source("tests/testthat.R")
library(testthat)
test_dir(here::here("tests", "testthat"), reporter = "summary")
