# Tests UNITARIOS de R/07_inference.R.
# Se comprueban contra (a) los resultados publicados en el Tema 6 (ejemplo de
# Forbes, datos MASS::forbes) y (b) las fórmulas de la inferencia del modelo
# lineal y logístico calculadas a mano.

forbes_fit <- function() {
  skip_if_not_installed("MASS")
  stats::lm(pres ~ bp, data = MASS::forbes)
}

# --- Reproducción del ejemplo 1 del Tema 6 (Forbes) ------------------------------

test_that("model_summary reproduce el resumen del ejemplo de Forbes", {
  s <- model_summary(forbes_fit())
  expect_equal(s$n, 17)
  expect_equal(s$r_squared, 0.9944, tolerance = 1e-4)
  expect_equal(s$adj_r_squared, 0.9941, tolerance = 1e-4)
  expect_equal(s$sigma, 0.2328, tolerance = 1e-3)
  expect_equal(s$f_statistic, 2677, tolerance = 1e-3)
  expect_equal(c(s$df1, s$df2), c(1, 15))
})

test_that("extract_coefficients reproduce la tabla de coeficientes de Forbes", {
  cf <- extract_coefficients(forbes_fit())
  expect_named(cf, c("term", "estimate", "std_error", "statistic", "p_value"))
  expect_equal(cf$estimate, c(-81.06373, 0.52289), tolerance = 1e-5)
  expect_equal(cf$std_error, c(2.05182, 0.01011), tolerance = 1e-3)
  expect_equal(cf$statistic, c(-39.51, 51.74), tolerance = 1e-3)
})

test_that("confidence_intervals reproduce confint() del Tema 6", {
  ci <- confidence_intervals(forbes_fit())
  expect_equal(ci$lower, c(-85.437080, 0.501352), tolerance = 1e-6)
  expect_equal(ci$upper, c(-76.6903740, 0.5444328), tolerance = 1e-6)
})

test_that("prediction_intervals reproduce las bandas para bp = 200 (Forbes)", {
  fit <- forbes_fit()
  conf <- prediction_intervals(fit, data.frame(bp = 200), "confidence")
  pred <- prediction_intervals(fit, data.frame(bp = 200), "prediction")
  expect_equal(conf$fit, 23.51475, tolerance = 1e-6)
  expect_equal(c(conf$lower, conf$upper), c(23.37862, 23.65089),
               tolerance = 1e-6)
  expect_equal(c(pred$lower, pred$upper), c(23.00016, 24.02935),
               tolerance = 1e-6)
})

# --- Fórmulas de la inferencia --------------------------------------------------

test_that("el IC del modelo lineal es b +/- t(1 - a/2; n - p) * ee", {
  fit <- fit_main_model(simulated_model_data(), log_power ~ log_rpm_c +
                          temp_nacelle_c)
  cf <- extract_coefficients(fit)
  ci <- confidence_intervals(fit, level = 0.9)
  q <- stats::qt(0.95, fit$df.residual)
  expect_equal(ci$lower, cf$estimate - q * cf$std_error)
  expect_equal(ci$upper, cf$estimate + q * cf$std_error)
})

test_that("test_coefficient con offset coincide con el contraste t calculado a mano", {
  d <- simulated_model_data(b = 3.2)
  fit <- fit_main_model(d, log_power ~ log_rpm_c + I(log_rpm_c^2) +
                          temp_nacelle_c)
  tt <- test_coefficient(fit, "log_rpm_c", 3)
  cf <- summary(fit)$coefficients["log_rpm_c", ]
  t_manual <- (cf[[1]] - 3) / cf[[2]]
  expect_equal(tt$estimate, cf[[1]])
  expect_equal(tt$std_error, cf[[2]])
  expect_equal(tt$statistic, t_manual)
  expect_equal(tt$p_value, 2 * stats::pt(-abs(t_manual), fit$df.residual))
  # Con valor 0 es el contraste habitual de summary()
  expect_equal(test_coefficient(fit, "log_rpm_c", 0)$statistic, cf[[3]])
  expect_error(test_coefficient(fit, "no_existe", 3), "no es una variable")
})

test_that("term_tests de un factor equivale a comparar modelos anidados con anova()", {
  d <- simulated_model_data(years = c(2018, 2019, 2020))
  full <- fit_main_model(d, log_power ~ log_rpm_c + year_f)
  reduced <- fit_main_model(d, log_power ~ log_rpm_c)
  a <- stats::anova(reduced, full)
  tt <- term_tests(full)
  expect_equal(tt$df[tt$term == "year_f"], 2)
  expect_equal(tt$statistic[tt$term == "year_f"], a$F[2])
  expect_equal(tt$p_value[tt$term == "year_f"], a$`Pr(>F)`[2])
})

test_that("elasticity_curve es la derivada de log(P) respecto a log(RPM)", {
  d <- simulated_model_data()
  fit <- fit_main_model(d, log_power ~ log_rpm_c + I(log_rpm_c^2))
  b <- stats::coef(fit)
  e <- elasticity_curve(fit, c(110, 140, 200))
  expect_equal(e$elasticity[2], b[["log_rpm_c"]])
  expect_equal(e$std_error[2],
               summary(fit)$coefficients["log_rpm_c", "Std. Error"])
  # Derivada numérica sobre las predicciones
  h <- 1e-5
  nd <- function(r) data.frame(log_rpm_c = log(r / 140))
  num <- (stats::predict(fit, nd(200 * exp(h))) -
            stats::predict(fit, nd(200 * exp(-h)))) / (2 * h)
  expect_equal(e$elasticity[3], unname(num), tolerance = 1e-6)
  expect_true(all(e$lower < e$elasticity & e$elasticity < e$upper))
  r3 <- rpm_at_elasticity(fit, 3)
  expect_equal(elasticity_curve(fit, r3)$elasticity, 3)
})

test_that("sin término cuadrático la elasticidad es constante", {
  fit <- fit_main_model(simulated_model_data(), log_power ~ log_rpm_c)
  e <- elasticity_curve(fit, c(100, 200))
  expect_equal(e$elasticity[1], e$elasticity[2])
  expect_true(is.na(rpm_at_elasticity(fit, 3)))
})

test_that("interpret_effects transforma a % de potencia salvo los términos en log", {
  d <- simulated_model_data()
  fit <- fit_main_model(d, log_power ~ log_rpm_c + temp_nacelle_c)
  ef <- interpret_effects(fit)
  b <- stats::coef(fit)
  expect_false("(Intercept)" %in% ef$term)
  expect_equal(ef$effect[ef$term == "temp_nacelle_c"],
               100 * (exp(b[["temp_nacelle_c"]]) - 1))
  expect_equal(ef$effect[ef$term == "log_rpm_c"], b[["log_rpm_c"]])
  expect_equal(ef$scale, c("elasticidad", "% de potencia"))
})

# --- Modelo logístico --------------------------------------------------------------

simulate_logistic <- function(n = 3000, seed = 3) {
  set.seed(seed)
  x <- stats::rnorm(n)
  g <- factor(sample(c("a", "b"), n, replace = TRUE))
  eta <- -0.5 + 1.2 * x + 0.8 * (g == "b")
  data.frame(y = stats::rbinom(n, 1, stats::plogis(eta)), x = x, g = g)
}

test_that("en el glm se obtienen razones de odds e IC de Wald", {
  d <- simulate_logistic()
  fit <- fit_main_model(d, y ~ x + g, family = stats::binomial())
  ef <- interpret_effects(fit)
  cf <- extract_coefficients(fit)
  expect_equal(ef$effect, exp(cf$estimate[-1]))
  ci <- confidence_intervals(fit)
  expect_equal(ci$lower, cf$estimate - stats::qnorm(0.975) * cf$std_error)
  expect_lt(abs(cf$estimate[2] - 1.2), 4 * cf$std_error[2])
})

test_that("model_summary del glm da el contraste de razón de verosimilitudes", {
  fit <- fit_main_model(simulate_logistic(), y ~ x + g,
                        family = stats::binomial())
  s <- model_summary(fit)
  expect_equal(s$lr_statistic, fit$null.deviance - fit$deviance)
  expect_equal(s$lr_df, 2)
  expect_equal(s$aic, stats::AIC(fit))
  expect_equal(s$dispersion, 1, tolerance = 0.1)   # datos binarios simulados
  expect_true(s$deviance_explained > 0 && s$deviance_explained < 1)
  tt <- term_tests(fit)
  expect_equal(tt$term, c("x", "g"))
})

test_that("las probabilidades predichas y sus bandas están en [0, 1]", {
  d <- simulate_logistic()
  fit <- fit_main_model(d, y ~ x + g, family = stats::binomial())
  p <- prediction_intervals(fit, data.frame(x = c(-5, 0, 5), g = "a"))
  expect_true(all(p$lower >= 0 & p$upper <= 1))
  expect_true(all(p$lower <= p$fit & p$fit <= p$upper))
})

# --- Comparación entre muestras y residuos -----------------------------------------

test_that("compare_fits_across_samples devuelve una fila por muestra y término", {
  d <- simulated_model_data()
  f <- log_power ~ log_rpm_c
  cmp <- compare_fits_across_samples(list(
    "Completa" = fit_main_model(d, f),
    "Mitad" = fit_main_model(d[seq(1, nrow(d), 2), ], f)))
  expect_equal(nrow(cmp), 4)
  expect_equal(levels(cmp$sample), c("Completa", "Mitad"))
  expect_equal(cmp$n[cmp$sample == "Mitad"][1], ceiling(nrow(d) / 2))
  expect_error(compare_fits_across_samples(list(fit_main_model(d, f))))
})

test_that("residual_lag1 detecta errores autocorrelacionados", {
  set.seed(5)
  n <- 2000
  x <- stats::rnorm(n)
  e_ar <- as.numeric(stats::arima.sim(list(ar = 0.8), n))
  t <- seq_len(n)
  indep <- stats::lm(y ~ x, data.frame(y = x + stats::rnorm(n), x = x))
  ar <- stats::lm(y ~ x, data.frame(y = x + e_ar, x = x))
  expect_lt(abs(residual_lag1(indep, t)), 0.1)
  expect_gt(residual_lag1(ar, t), 0.6)
})

test_that("power_curve_grid crece con las RPM y la banda de predicción contiene la de confianza", {
  d <- simulated_model_data()
  d$year_f <- year_factor(d$year)
  fit <- fit_main_model(d, log_power ~ log_rpm_c + I(log_rpm_c^2) +
                          temp_nacelle_c + year_f + day_period)
  g <- power_curve_grid(fit, d, n = 20, settings = model_settings(
    list(analysis = list(model = NULL))))
  expect_equal(nrow(g), 20)
  expect_false(is.unsorted(g$fit_w))
  expect_true(all(g$pred_lower_w <= g$conf_lower_w &
                    g$conf_upper_w <= g$pred_upper_w))
})

# --- Formato de tablas ----------------------------------------------------------------

test_that("fmt_p y term_label siguen las convenciones del informe", {
  expect_equal(fmt_p(c(0.0001, 0.0456, NA)), c("< 0,001", "0,046", "--"))
  expect_equal(term_label(c("(Intercept)", "log_rpm_c", "I(log_rpm_c^2)",
                            "year_f2020", "day_period12-18 h",
                            "seasonInvierno", "year_f")),
               c("Constante", "log(RPM/140)", "log(RPM/140)^2",
                 "Año 2020 (vs 2018)", "Franja 12-18 h (vs 0-6 h)",
                 "Invierno (vs verano)", "Año"))
})
