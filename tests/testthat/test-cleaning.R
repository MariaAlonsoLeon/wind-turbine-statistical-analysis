raw_fixture <- function() load_all_years(fixture_files())

test_that("se parsean marcas con separador de ms '.' y ','", {
  x <- tibble::tibble(source_file = "f",
                      log_time_raw = c("2017:12:22:10:36:00.589",
                                       "2017:12:22:11:03:56,127"),
                      inv_time_raw = c("1513939000", "1513940700"))
  p <- parse_datetime_columns(x)
  expect_false(anyNA(p$log_time))
  expect_s3_class(p$log_time, "POSIXct")
  expect_equal(format(p$log_time, "%Y-%m-%d %H:%M:%S"),
               c("2017-12-22 10:36:00", "2017-12-22 11:03:56"))
})

test_that("las correcciones técnicas no pierden filas válidas y ordenan en el tiempo", {
  clean <- clean_technical(raw_fixture())
  expect_equal(nrow(clean), 45)
  expect_false(anyNA(clean$log_time))
  expect_false(is.unsorted(clean$log_time))
  expect_true(all(c("step", "n_removed") %in% names(attr(clean, "cleaning_log"))))
})

test_that("solo se eliminan duplicados EXACTOS", {
  raw <- raw_fixture()
  dup_exact <- raw[1, ]
  dup_value <- raw[2, ]
  dup_value$power_out_w <- dup_value$power_out_w + 1   # misma hora, otro valor
  clean <- clean_technical(dplyr::bind_rows(raw, dup_exact, dup_value))
  expect_equal(nrow(clean), nrow(raw) + 1)
  expect_equal(detect_duplicated_timestamps(clean)$n_rows_sharing_timestamp, 2)
})

test_that("build_analysis_dataset no filtra nada con los valores por defecto", {
  clean <- clean_technical(raw_fixture())
  final <- build_analysis_dataset(clean)
  expect_equal(nrow(final), nrow(clean))
  expect_true(all(attr(final, "cleaning_log")$n_removed == 0))
})

test_that("los filtros metodológicos explícitos funcionan y quedan registrados", {
  clean <- clean_technical(raw_fixture())
  by_period <- build_analysis_dataset(clean, period_start = "2018-01-01")
  expect_true(all(by_period$file_year == 2018))
  expect_equal(attr(by_period, "cleaning_log")$n_removed[1], 25)

  codes <- unique(clean$turbine_status)[1]
  by_status <- build_analysis_dataset(clean, turbine_status_keep = codes)
  expect_true(all(by_status$turbine_status == codes))
})

test_that("la agregación horaria conserva el número total de registros", {
  clean <- clean_technical(raw_fixture())
  hourly <- aggregate_time(clean, "hour")
  expect_lt(nrow(hourly), nrow(clean))
  expect_equal(sum(hourly$n_records), nrow(clean))
})
