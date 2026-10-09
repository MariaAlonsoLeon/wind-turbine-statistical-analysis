mk_times <- function(offsets_s) as.POSIXct("2017-12-31 23:50:00", tz = "UTC") + offsets_s

test_that("gap_accounting reparte exactamente el periodo y cuenta huecos que cruzan el año una vez", {
  d <- tibble::tibble(log_time = mk_times(c(0, 60, 120, 1320, 1380, 4980)),
                      file_year = c(2017L, 2017L, 2017L, 2018L, 2018L, 2018L))
  ga <- gap_accounting(d, gap_threshold = 180, nominal_s = 60)
  expect_equal(ga$identity_error_s, 0)
  expect_equal(ga$n_gaps, 2)
  expect_equal(ga$gap_hours, (1200 + 3600) / 3600)
  expect_equal(ga$gap_hours_net, (1140 + 3540) / 3600)
  expect_equal(ga$n_gaps_crossing_year, 1)
  gaps <- detect_time_gaps(d, 180)
  expect_equal(sum(gaps$crosses_year), 1)
  expect_equal(gaps$file_year_after[gaps$crosses_year], 2018L)
})

test_that("flag_episodes separa rachas por valores y por huecos", {
  flag <- c(FALSE, TRUE, TRUE, FALSE, TRUE, TRUE, TRUE)
  seg <- c(1, 1, 1, 1, 1, 2, 2)
  expect_equal(flag_episodes(flag, seg), c(NA, 1, 1, NA, 2, 3, 3))
})

test_that("high_power_check cuenta registros, episodios y excesos sobre Power max", {
  d <- tibble::tibble(
    log_time = mk_times(60 * 0:5), year = 2017L,
    power_out_w = c(0, 1900, 2000, 500, 2500, 100),
    power_max_w = 2400, segment_id = 1, turbine_status = c(1, 0, 0, 0, 8, 0),
    grid_status = 0, system_status = 0, rpm = 300, power_reg_w = 0)
  h <- high_power_check(d, threshold = 1800)
  expect_equal(h$summary$n_above, 3)
  expect_equal(h$summary$n_episodes, 2)
  expect_equal(h$summary$max_power, 2500)
  expect_equal(h$summary$n_above_power_max, 1)
  expect_equal(nrow(d), 6)   # no elimina nada
})

test_that("compare_power_measures calcula diferencias y extremos", {
  d <- tibble::tibble(log_time = mk_times(60 * 0:3), power_out_w = c(0, 100, 200, 300),
                      power_reg_w = c(0, 90, 260, 300), rpm = 1, turbine_status = c(1, 0, 0, 0),
                      grid_status = 0, starts_after_gap = FALSE)
  cmp <- compare_power_measures(d, n_extremes = 2)
  expect_equal(cmp$summary$min_diff, -60)
  expect_equal(cmp$summary$max_diff, 10)
  expect_equal(cmp$extremes$diff[1], -60)
  expect_equal(sum(cmp$by_status$n), 4)
})

test_that("summarise_monthly incluye meses vacíos con cobertura 0", {
  d <- tibble::tibble(log_time = as.POSIXct(c("2018-01-01 00:00", "2018-03-01 00:00"), tz = "UTC"),
                      power_out_w = c(10, 20))
  m <- summarise_monthly(d)
  expect_equal(nrow(m), 3)
  expect_equal(m$n_records, c(1, 0, 1))
  expect_equal(m$coverage_pct[1], 100 / (31 * 1440))
})

test_that("clean_technical registra los retrocesos del orden original", {
  clean <- clean_technical(load_all_years(fixture_files()))
  to <- attr(clean, "time_order")
  expect_equal(nrow(to), 2)
  expect_true(all(to$n_backward == 0))
})
