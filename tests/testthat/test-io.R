test_that("detecta los ficheros anuales y extrae el año del nombre", {
  files <- fixture_files()
  expect_equal(files$year, c(2017L, 2018L))
  expect_true(all(file.exists(files$path)))
})

test_that("falla con un mensaje claro si no hay ficheros", {
  expect_error(list_raw_files(tempdir(), fixture_pattern), "data/README.md")
})

test_that("la cabecera se lee sin espacios exteriores (' T1' -> 'T1')", {
  header <- read_raw_header(fixture_files()$path[1])
  expect_length(header, 39)
  expect_true("T1" %in% header)
  expect_false(any(header != trimws(header)))
})

test_that("el esquema documentado es compatible con los ficheros válidos", {
  check <- validate_schema(fixture_files())
  expect_true(all(check$compatible))
  expect_equal(check$names_with_spaces[1], " T1")
})

test_that("la validación de esquema detecta columnas que faltan y detiene la carga", {
  bad <- list_raw_files(fixture_dir("bad_schema"), fixture_pattern)
  check <- validate_schema(bad)
  expect_false(check$compatible)
  expect_match(check$missing_columns, "Timer")
  expect_error(load_all_years(bad), "incompatible")
})

test_that("load_year_data devuelve las columnas esperadas con tipos correctos", {
  d <- load_year_data(fixture_files()$path[1])
  expect_equal(nrow(d), 25)
  expect_setequal(names(d), c("source_file", scada_schema()$name))
  expect_type(d$log_time_raw, "character")
  numeric_cols <- setdiff(scada_schema()$name, c("log_time_raw", "inv_time_raw"))
  expect_true(all(vapply(d[numeric_cols], is.numeric, logical(1))))
  expect_equal(nrow(attr(d, "parse_problems")), 0)
})

test_that("load_all_years combina los años y conserva el origen", {
  files <- fixture_files()
  d <- load_all_years(files)
  expect_equal(nrow(d), 25 + 20)
  expect_equal(sort(unique(d$file_year)), c(2017L, 2018L))
  expect_equal(as.vector(table(d$source_file)), c(25L, 20L))
})
