test_that("la paleta tiene tres colores funcionales más un tinte neutro", {
  pal <- wind_palette()
  expect_named(pal, c("primary", "secondary", "neutral", "neutral_light"))
  expect_true(all(grepl("^#[0-9A-Fa-f]{6}$", pal)))
})

test_that("theme_wind_report es un tema ggplot sin títulos ni pies", {
  th <- theme_wind_report()
  expect_s3_class(th, "theme")
  expect_s3_class(th$plot.caption, "element_blank")
  expect_s3_class(th$panel.grid.minor, "element_blank")
})

test_that("las figuras del informe usan el tema y no llevan caption interno", {
  d <- tibble::tibble(power_out_w = c(0, 10, 500, 1900), rpm = c(0, 120, 250, 330),
                      windspeed_ref_ms = c(0, 3, 6, 9))
  for (p in list(plot_power_distribution(d), plot_power_vs_continuous(d),
                 plot_power_by_level(d))) {
    expect_s3_class(p, "ggplot")
    expect_null(p$labels$caption)
  }
})

test_that("looks_numeric distingue números formateados de texto", {
  expect_true(looks_numeric(c("1.234", "5,6", "-12", "75,1 %")))
  expect_false(looks_numeric(c("Power out", "12")))
})

test_that("escape_caption protege los caracteres especiales de LaTeX", {
  expect_equal(escape_caption("a_b 5 % & #"), "a\\_b 5 \\% \\& \\#")
})

test_that("style_report_table devuelve una tabla fuera de LaTeX con alineación automática", {
  tab <- style_report_table(data.frame(Variable = "x", Valor = "1,5"),
                            caption = "Prueba", note = "Nota")
  expect_true(any(grepl("Prueba", tab)))
  expect_equal(attr(tab, "note"), "Nota")
})
