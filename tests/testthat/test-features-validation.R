processed_fixture <- function() add_features(clean_technical(load_all_years(fixture_files())))

test_that("las variables derivadas no filtran filas y marcan los huecos", {
  d <- processed_fixture()
  expect_equal(nrow(d), 45)
  expect_true(all(c("year", "hour", "segment_id", "energy_increment_wh",
                    "producing") %in% names(d)))
  # hay un hueco entre el bloque inicial y las filas posteriores de 2017
  expect_gt(max(d$segment_id), 1)
  expect_true(all(is.na(d$energy_increment_wh[d$starts_after_gap])))
})

test_that("los incrementos negativos del contador de energía se conservan", {
  d <- tibble::tibble(log_time = as.POSIXct("2017-12-01", tz = "UTC") + 60 * 0:3,
                      energy_wh_cum = c(10, 11, 10, 12))
  d <- add_energy_increment(add_sampling_features(d))
  expect_equal(d$energy_increment_wh, c(NA, 1, -1, 2))
})

test_that("las constantes documentadas se cumplen en los datos de prueba", {
  d <- processed_fixture()
  expect_true(all(check_documented_constants(d)$n_different == 0))
})

test_that("el inventario de códigos suma el 100 % por variable", {
  inv <- status_code_inventory(processed_fixture())
  totals <- tapply(inv$pct, inv$variable, sum)
  expect_true(all(abs(totals - 100) < 1e-9))
})

test_that("la detección de huecos encuentra un hueco sintético", {
  d <- tibble::tibble(log_time = as.POSIXct("2017-12-01", tz = "UTC") +
                        c(0, 60, 120, 3720, 3780))
  gaps <- detect_time_gaps(d, gap_threshold = 180)
  expect_equal(nrow(gaps), 1)
  expect_equal(gaps$duration_hours, 1)
})
