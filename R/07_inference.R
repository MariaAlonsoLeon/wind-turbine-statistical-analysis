# =============================================================================
# 07_inference.R — Inferencia e interpretación de coeficientes
# -----------------------------------------------------------------------------
# Responsable: ÁLVARO                                  ESTADO: implementado
#
# Entrada común: un modelo ajustado por fit_main_model() (06_modeling.R).
# Salida: tibbles que 10_tables.R formatea para el informe.
# =============================================================================

#' ¿El modelo es un glm? (los lm devuelven también clase "lm")
is_glm <- function(model) inherits(model, "glm")

#' Resumen global del ajuste
#' @param model Objeto lm/glm.
#' @return tibble de una fila. lm: n, nº de parámetros, R2, R2 ajustado,
#'   error estándar residual, contraste F global y p-valor. glm: n, nº de
#'   parámetros, devianzas nula y residual, AIC, dispersión (Pearson) y
#'   contraste de razón de verosimilitudes frente al modelo nulo.
model_summary <- function(model) {
  s <- summary(model)
  n <- stats::nobs(model)
  p <- length(stats::coef(model))
  if (is_glm(model)) {
    lr <- model$null.deviance - model$deviance
    df <- model$df.null - model$df.residual
    return(tibble::tibble(
      model_type = "glm", n = n, n_params = p,
      null_deviance = model$null.deviance, residual_deviance = model$deviance,
      df_residual = model$df.residual, aic = stats::AIC(model),
      dispersion = sum(stats::residuals(model, type = "pearson")^2) /
        model$df.residual,
      deviance_explained = 1 - model$deviance / model$null.deviance,
      lr_statistic = lr, lr_df = df,
      p_value = stats::pchisq(lr, df, lower.tail = FALSE)
    ))
  }
  f <- s$fstatistic
  tibble::tibble(
    model_type = "lm", n = n, n_params = p,
    r_squared = s$r.squared, adj_r_squared = s$adj.r.squared,
    sigma = s$sigma, df_residual = model$df.residual,
    f_statistic = unname(f["value"]), df1 = unname(f["numdf"]),
    df2 = unname(f["dendf"]),
    p_value = stats::pf(f["value"], f["numdf"], f["dendf"],
                        lower.tail = FALSE)[[1]]
  )
}

#' Tabla de coeficientes
#' @return tibble: term, estimate, std_error, statistic, p_value (estadístico
#'   t en lm, z de Wald en glm).
extract_coefficients <- function(model) {
  cf <- summary(model)$coefficients
  tibble::tibble(term = rownames(cf), estimate = unname(cf[, 1]),
                 std_error = unname(cf[, 2]), statistic = unname(cf[, 3]),
                 p_value = unname(cf[, 4]))
}

#' Intervalos de confianza de los coeficientes
#' lm: intervalos t exactos bajo normalidad (confint). glm: intervalos de
#' Wald (confint.default). Con más de 10^5 observaciones los intervalos de
#' perfil de verosimilitud son prácticamente iguales y mucho más lentos.
#'
#' @param level Nivel de confianza (0.95 por defecto).
#' @return tibble: term, lower, upper.
confidence_intervals <- function(model, level = 0.95) {
  ci <- if (is_glm(model)) stats::confint.default(model, level = level)
        else stats::confint(model, level = level)
  tibble::tibble(term = rownames(ci), lower = unname(ci[, 1]),
                 upper = unname(ci[, 2]))
}

#' Contrastes F (lm) o de razón de verosimilitudes (glm) de cada término
#' drop1() compara el modelo con el que resulta de eliminar cada término
#' manteniendo los demás. Para un factor (año, franja) contrasta
#' conjuntamente todos sus coeficientes, que es lo que interesa.
#'
#' @return tibble: term, df, statistic, p_value.
term_tests <- function(model) {
  test <- if (is_glm(model)) "Chisq" else "F"
  d <- stats::drop1(model, test = test)
  d <- d[rownames(d) != "<none>", ]
  stat <- if (is_glm(model)) d[["LRT"]] else d[["F value"]]
  tibble::tibble(term = rownames(d), df = d[["Df"]], statistic = stat,
                 p_value = d[[ncol(d)]])
}

#' Contraste H0: beta_term = value con el argumento offset de lm()
#' Se reajusta el modelo añadiendo offset(value * x): el coeficiente de x pasa
#' a estimar beta - value y su contraste t de summary() es exactamente el de
#' H0: beta = value (Tema 6, apartado 6.3).
#'
#' @param term Nombre de una variable numérica del modelo.
#' @return tibble: term, null_value, estimate, std_error, statistic, df,
#'   p_value.
test_coefficient <- function(model, term, value) {
  mf <- stats::model.frame(model)
  if (!term %in% names(mf)) {
    stop("test_coefficient(): '", term, "' no es una variable del modelo.",
         call. = FALSE)
  }
  f <- stats::update(stats::formula(model),
                     stats::as.formula(paste0(". ~ . + offset(", value, " * ",
                                              term, ")")))
  refit <- stats::lm(f, data = mf)
  cf <- summary(refit)$coefficients[term, ]
  tibble::tibble(term = term, null_value = value,
                 estimate = cf[[1]] + value, std_error = cf[[2]],
                 statistic = cf[[3]], df = refit$df.residual,
                 p_value = cf[[4]])
}

#' Elasticidad de la potencia respecto a las RPM a lo largo de la curva
#' En log(P) = b0 + b1 * l + b2 * l^2 + ..., con l = log(RPM / ref), la
#' elasticidad es dlog(P)/dlog(RPM) = b1 + 2 * b2 * l. Su varianza se obtiene
#' de vcov(): Var(b1) + 4 l^2 Var(b2) + 4 l Cov(b1, b2).
#'
#' @param rpm Velocidades de giro en las que se evalúa.
#' @return tibble: rpm, elasticity, std_error, lower, upper.
elasticity_curve <- function(model, rpm, reference_rpm = 140,
                             linear = "log_rpm_c",
                             quadratic = "I(log_rpm_c^2)", level = 0.95) {
  b <- stats::coef(model)
  v <- stats::vcov(model)
  l <- log(rpm / reference_rpm)
  has_q <- quadratic %in% names(b)
  b2 <- if (has_q) b[[quadratic]] else 0
  est <- b[[linear]] + 2 * b2 * l
  var <- v[linear, linear]
  if (has_q) var <- var + 4 * l^2 * v[quadratic, quadratic] +
    4 * l * v[linear, quadratic]
  se <- sqrt(var)
  q <- stats::qt(1 - (1 - level) / 2, model$df.residual)
  tibble::tibble(rpm = rpm, elasticity = est, std_error = se,
                 lower = est - q * se, upper = est + q * se)
}

#' Velocidad de giro a la que la elasticidad estimada vale `value`
#' Resuelve b1 + 2 b2 log(r / ref) = value (solo con término cuadrático).
rpm_at_elasticity <- function(model, value = 3, reference_rpm = 140,
                              linear = "log_rpm_c",
                              quadratic = "I(log_rpm_c^2)") {
  b <- stats::coef(model)
  if (!quadratic %in% names(b)) return(NA_real_)
  reference_rpm * exp((value - b[[linear]]) / (2 * b[[quadratic]]))
}

#' Interpretación de los coeficientes en escala original
#'
#' * lm con log(P): un coeficiente b de una predictora en escala original
#'   implica que P se multiplica por exp(b), es decir, cambia un
#'   100 * (exp(b) - 1) % por unidad 
#' * glm logístico: razones de odds exp(b).
#' Los extremos del intervalo se transforman igual (transformación monótona).
#'
#' @return tibble: term, scale, effect, lower, upper.
interpret_effects <- function(model, level = 0.95,
                              log_terms = c("log_rpm_c", "I(log_rpm_c^2)",
                                            "log_rpm")) {
  cf <- extract_coefficients(model)
  ci <- confidence_intervals(model, level)
  d <- dplyr::left_join(cf, ci, by = "term")
  d <- d[d$term != "(Intercept)", ]
  if (is_glm(model)) {
    return(tibble::tibble(term = d$term, scale = "odds ratio",
                          effect = exp(d$estimate), lower = exp(d$lower),
                          upper = exp(d$upper)))
  }
  pct <- function(x) 100 * (exp(x) - 1)
  is_log <- d$term %in% log_terms
  tibble::tibble(
    term = d$term,
    scale = ifelse(is_log, "elasticidad", "% de potencia"),
    effect = ifelse(is_log, d$estimate, pct(d$estimate)),
    lower = ifelse(is_log, d$lower, pct(d$lower)),
    upper = ifelse(is_log, d$upper, pct(d$upper))
  )
}

#' Compara los mismos coeficientes ajustados en distintas muestras
#' Se usa para el análisis de sensibilidad: muestra completa, submuestra
#' espaciada (dependencia temporal) e intervalos con turbine_status = 0
#' (estados operativos).

#' @param models Lista nombrada de modelos con la misma fórmula.
#' @return tibble largo: sample, n, term, estimate, std_error, p_value.
compare_fits_across_samples <- function(models) {
  if (is.null(names(models))) stop("models debe ser una lista nombrada",
                                   call. = FALSE)
  out <- lapply(names(models), function(nm) {
    cf <- extract_coefficients(models[[nm]])
    tibble::tibble(sample = nm, n = stats::nobs(models[[nm]]), term = cf$term,
                   estimate = cf$estimate, std_error = cf$std_error,
                   p_value = cf$p_value)
  })
  out <- dplyr::bind_rows(out)
  out$sample <- factor(out$sample, levels = names(models))
  out
}

#' Autocorrelación de orden 1 de los residuos, en orden temporal
#'
#' Resumen descriptivo para justificar la separación de la submuestra
#' espaciada. El diagnóstico formal (Durbin-Watson, ACF) está en
#' 08_diagnostics.R.
residual_lag1 <- function(model, time) {
  r <- stats::residuals(model)[order(time)]
  stats::cor(r[-1], r[-length(r)])
}

#' Intervalos de confianza / predicción para nuevos valores
#'
#' lm: predict(interval = ...) en la escala del modelo (log W) y vuelta a W
#' con exp(). Como la respuesta está en logaritmos, exp(ajuste) estima la
#' MEDIANA de la potencia condicionada (no la media) y el intervalo de
#' predicción es asimétrico en W.
#' glm: predicción en la escala logit con su error estándar e intervalo de
#' Wald, transformado con la función logística (siempre dentro de [0, 1]).
#'
#' @param newdata data.frame con los valores de las predictoras.
#' @param interval "confidence" o "prediction" (solo lm).
#' @return newdata con fit, lower, upper (y fit_w, lower_w, upper_w en lm).
prediction_intervals <- function(model, newdata, interval = "confidence",
                                 level = 0.95) {
  newdata <- as.data.frame(newdata)
  if (is_glm(model)) {
    p <- stats::predict(model, newdata, type = "link", se.fit = TRUE)
    z <- stats::qnorm(1 - (1 - level) / 2)
    newdata$fit <- stats::plogis(p$fit)
    newdata$lower <- stats::plogis(p$fit - z * p$se.fit)
    newdata$upper <- stats::plogis(p$fit + z * p$se.fit)
    return(tibble::as_tibble(newdata))
  }
  interval <- match.arg(interval, c("confidence", "prediction"))
  p <- stats::predict(model, newdata, interval = interval, level = level)
  newdata$fit <- p[, "fit"]
  newdata$lower <- p[, "lwr"]
  newdata$upper <- p[, "upr"]
  newdata$fit_w <- exp(newdata$fit)
  newdata$lower_w <- exp(newdata$lower)
  newdata$upper_w <- exp(newdata$upper)
  tibble::as_tibble(newdata)
}

#' Valores de referencia de las predictoras (para curvas y perfiles)
#'
#' Temperatura: mediana. Factores: su nivel de referencia (primer nivel).
reference_values <- function(data) {
  list(temp_nacelle_c = stats::median(data$temp_nacelle_c, na.rm = TRUE),
       year_f = factor(levels(data$year_f)[1], levels = levels(data$year_f)),
       day_period = factor(levels(data$day_period)[1],
                           levels = levels(data$day_period)))
}

#' Curva de potencia estimada con bandas de confianza y de predicción
#'
#' Rejilla de RPM entre los percentiles 1 y 99 del conjunto del modelo; el
#' resto de predictoras en sus valores de referencia.
#'
#' @return tibble: rpm, fit_w, conf_lower_w, conf_upper_w, pred_lower_w,
#'   pred_upper_w.
power_curve_grid <- function(model, data, n = 100,
                             settings = model_settings()) {
  r <- stats::quantile(data$rpm, c(0.01, 0.99), names = FALSE)
  ref <- reference_values(data)
  grid <- data.frame(rpm = seq(r[1], r[2], length.out = n))
  grid$log_rpm_c <- log(grid$rpm / settings$reference_rpm)
  grid$log_rpm <- log(grid$rpm)
  grid$temp_nacelle_c <- ref$temp_nacelle_c
  grid$year_f <- ref$year_f
  grid$day_period <- ref$day_period
  conf <- prediction_intervals(model, grid, "confidence")
  pred <- prediction_intervals(model, grid, "prediction")
  tibble::tibble(rpm = grid$rpm, fit_w = conf$fit_w,
                 conf_lower_w = conf$lower_w, conf_upper_w = conf$upper_w,
                 pred_lower_w = pred$lower_w, pred_upper_w = pred$upper_w)
}

#' Probabilidad estimada de producción por franja del día y estación
#'
#' Temperatura en su mediana y año de referencia.
production_profile <- function(model, data) {
  ref <- reference_values(data)
  grid <- expand.grid(day_period = levels(data$day_period),
                      season = levels(data$season))
  grid$day_period <- factor(grid$day_period, levels = levels(data$day_period))
  grid$season <- factor(grid$season, levels = levels(data$season))
  grid$temp_nacelle_c <- ref$temp_nacelle_c
  grid$year_f <- ref$year_f
  prediction_intervals(model, grid)
}
