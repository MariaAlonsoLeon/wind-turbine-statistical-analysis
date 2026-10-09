test_that("leer y limpiar NO modifica los ficheros originales (copia temporal)", {
  tmp <- file.path(tempdir(), "raw_integrity")
  dir.create(tmp, showWarnings = FALSE)
  file.copy(fixture_files()$path, tmp, overwrite = TRUE)
  files <- list_raw_files(tmp, fixture_pattern)
  before <- raw_manifest(files)
  invisible(add_features(clean_technical(load_all_years(files))))
  expect_identical(raw_manifest(files)$md5, before$md5)
})

test_that("con los datos reales: el conjunto no está vacío y data/raw no cambia", {
  raw_dir <- here::here("data", "raw")
  skip_if(length(list.files(raw_dir, fixture_pattern)) == 0,
          "Sin datos reales en data/raw")
  files <- list_raw_files(raw_dir, fixture_pattern)
  before <- raw_manifest(files)
  d <- clean_technical(load_all_years(files))
  expect_gt(nrow(d), 0)
  expect_true(all(validate_schema(files)$compatible))
  expect_false(anyNA(d$log_time))
  expect_identical(raw_manifest(files)$md5, before$md5)
})
