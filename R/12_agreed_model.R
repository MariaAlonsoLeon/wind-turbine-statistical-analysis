# =============================================================================
# 12_agreed_model.R — Modelo acordado por el grupo (MVP)
# -----------------------------------------------------------------------------
# Responsable: ÁLVARO                                  ESTADO: implementado
#
# Versión del modelo acordada tras la revisión del grupo:
#   * Respuesta: Power out; Power reg solo como comprobación (D-M1).
#   * Periodo 2018-2022 (D-M2): 2017 solo cubre noviembre-diciembre.
#   * Población: intervalos de 10 min en los que la turbina está produciendo
#     (D-M4, D-M6). La pregunta queda condicionada a producción positiva.
#   * Turbine status = 0 NO se usa como filtro ni se interpreta como
#     "funcionamiento normal" (el diccionario no documenta los códigos); solo
#     como criterio empírico en el análisis de sensibilidad (D-M3).
#   * Predictoras: RPM (con término cuadrático), temperatura en la góndola y
#     estación del año (época del año). Año y franja del día solo como
#     variables de control en la sensibilidad.
#   * Dependencia temporal (D-M5): los huecos no se eliminan (son ausencia de
#     observaciones). Se compara una submuestra ESPACIADA en el tiempo con una
#     submuestra ALEATORIA de 5.000 intervalos, que no rompe la dependencia
#     porque deja muchas observaciones próximas en el tiempo.
#
# Pregunta: entre los periodos en los que la turbina genera potencia, ¿cómo
# se relaciona la potencia de salida con la velocidad de giro y, para una
# misma velocidad de giro, existen diferencias asociadas a la temperatura o a
# la época del año?
#
# Reutiliza, sin modificarlas, las funciones de 06_modeling.R (preparación,
# ajuste, submuestra espaciada) y 07_inference.R (inferencia).
# =============================================================================

# -----------------------------------------------------------------------------
# Parámetros y datos
# -----------------------------------------------------------------------------

#' Parámetros del modelo acordado
#'
#' Los de model_settings() (intervalo, mínimo de minutos, RPM de referencia,
#' separación de la submuestra espaciada) más el tamaño y la semilla de la
#' submuestra aleatoria. Si config.yml incluye analysis$agreed, sus valores
#' prevalecen.
agreed_settings <- function(config = load_config()) {
  defaults <- utils::modifyList(model_settings(config),
                                list(random_n = 5000,
                                     seed = config$seed %||% 20261006))
  user <- config$analysis$agreed
  if (is.null(user)) return(defaults)
  utils::modifyList(defaults, user)
}

#' Conjunto del modelo acordado
#'
#' Es el conjunto de prepare_conditional_data() (intervalos de producción
#' sostenida) con la estación del año del hemisferio sur. Se conserva el
#' atributo "preparation_log".
prepare_agreed_data <- function(data, settings = agreed_settings(),
                                response = "power_out_w") {
  out <- prepare_conditional_data(data, settings, response)
  out$season <- season_south(out$month)
  out
}

#' Fórmulas del modelo acordado
#'
#' * a1-a4: modelos ANIDADOS. a1 ley potencial en RPM; a2 añade la curvatura
#'   (cuadrado de log(RPM/140)); a3 la temperatura en la góndola; a4 la
#'   estación del año. a4 es el modelo principal.
#' * a5: a4 con año y franja del día como variables de control, para ver si
#'   los efectos de temperatura y estación dependen de esos controles.
agreed_formulas <- function() {
  list(
    a1 = log_power ~ log_rpm_c,
    a2 = log_power ~ log_rpm_c + I(log_rpm_c^2),
    a3 = log_power ~ log_rpm_c + I(log_rpm_c^2) + temp_nacelle_c,
    a4 = log_power ~ log_rpm_c + I(log_rpm_c^2) + temp_nacelle_c + season,
    a5 = log_power ~ log_rpm_c + I(log_rpm_c^2) + temp_nacelle_c + season +
      year_f + day_period
  )
}

#' Submuestra aleatoria simple de n intervalos, ordenada en el tiempo
#'
#' Es la alternativa propuesta para romper la dependencia temporal. Se
#' incluye para comprobar que NO lo consigue: al elegir intervalos al azar
#' muchos quedan a pocos minutos u horas de otro intervalo elegido.
#'
#' @param seed Semilla (reproducibilidad); NULL para no fijarla. Con semilla,
#'   el estado del generador aleatorio de la sesión se restaura al terminar,
#'   para no alterar otros pasos aleatorios del análisis.
random_subsample <- function(data, n = 5000, seed = NULL) {
  assert_columns(data, "log_time", "random_subsample")
  if (n >= nrow(data)) {
    return(data[order(data$log_time), , drop = FALSE])
  }
  if (!is.null(seed)) {
    old_seed <- if (exists(".Random.seed", envir = globalenv())) {
      get(".Random.seed", envir = globalenv())
    }
    on.exit({
      if (is.null(old_seed)) {
        rm(".Random.seed", envir = globalenv())
      } else {
        assign(".Random.seed", old_seed, envir = globalenv())
      }
    })
    set.seed(seed)
  }
  out <- data[sample(nrow(data), n), , drop = FALSE]
  out[order(out$log_time), , drop = FALSE]
}

# -----------------------------------------------------------------------------
# Ajuste
# -----------------------------------------------------------------------------

#' Prepara los datos y ajusta todos los modelos del análisis acordado
#'
#' * candidates: a1-a4 sobre las mismas filas (para 09_model_selection.R).
#' * main: a4, modelo principal; controls: a5.
#' * Sensibilidad del modelo principal: submuestra espaciada y aleatoria
#'   (dependencia temporal, D-M5), intervalos con turbine_status = 0 en todos
#'   sus minutos (D-M3) y Power reg como respuesta (D-M1).
#' * times: marcas temporales de cada muestra, en el orden de los residuos.
#'
#' @param data Conjunto de análisis por minuto (load_analysis_data("final")).
fit_agreed_models <- function(data, settings = agreed_settings(),
                              response = "power_out_w") {
  f <- agreed_formulas()
  d <- prepare_agreed_data(data, settings, response)
  candidates <- fit_candidate_models(d, f[paste0("a", 1:4)])
  thin <- thin_sample(d, settings$thinning_hours)
  rand <- random_subsample(d, settings$random_n, settings$seed)
  status0 <- d[d$status0_share == 1, ]
  d_reg <- prepare_agreed_data(data, settings, "power_reg_w")
  list(
    settings = settings,
    data = d,
    candidates = candidates,
    main = candidates$a4,
    controls = fit_main_model(d, f$a5),
    thinned = fit_main_model(thin, f$a4),
    random = fit_main_model(rand, f$a4),
    status0 = fit_main_model(status0, f$a4),
    power_reg = fit_main_model(d_reg, f$a4),
    times = list(Completa = d$log_time, Espaciada = thin$log_time,
                 Aleatoria = rand$log_time)
  )
}

#' Lee los resultados del modelo acordado (scripts/04_fit_agreed_model.R)
#'
#' Si no existen, ejecuta el script, igual que load_model_results().
load_agreed_results <- function(dir = project_path("models_dir"),
                                build_if_missing = TRUE) {
  path <- file.path(dir, "agreed_results.rds")
  if (!file.exists(path)) {
    if (!build_if_missing) {
      stop("No existe ", path, ". Ejecuta scripts/04_fit_agreed_model.R",
           call. = FALSE)
    }
    message("No existe agreed_results.rds; se ajusta ahora el modelo acordado.")
    source(here::here("scripts", "04_fit_agreed_model.R"), local = new.env())
  }
  readRDS(path)
}

# -----------------------------------------------------------------------------
# Dependencia temporal de los residuos
# -----------------------------------------------------------------------------

#' Estadístico de Durbin-Watson de los residuos ordenados en el tiempo
#'
#' DW = sum((e_t - e_{t-1})^2) / sum(e_t^2); próximo a 2 sin autocorrelación
#' y a 0 con autocorrelación positiva fuerte (DW ~ 2 (1 - r1)). Es el mismo
#' estadístico que lmtest::dwtest() (Tema 6); aquí se usa solo como resumen
#' descriptivo, el contraste formal está en 08_diagnostics.R.
durbin_watson <- function(model, time) {
  e <- stats::residuals(model)
  if (length(e) != length(time)) {
    stop("durbin_watson(): residuos y tiempos con distinta longitud.",
         call. = FALSE)
  }
  e <- e[order(time)]
  sum(diff(e)^2) / sum(e^2)
}

#' Dependencia temporal de los residuos en varias muestras
#'
#' @param models,times Listas con los mismos nombres: modelo y marcas
#'   temporales de sus observaciones.
#' @return tibble: sample, n, median_gap_h (separación mediana entre
#'   observaciones consecutivas), pct_gap_1h (% de pares consecutivos a menos
#'   de 1 h), lag1, dw.
dependence_summary <- function(models, times) {
  if (!identical(names(models), names(times))) {
    stop("dependence_summary(): models y times deben tener los mismos nombres.",
         call. = FALSE)
  }
  out <- lapply(names(models), function(nm) {
    t <- sort(as.numeric(times[[nm]]))
    gap_h <- diff(t) / 3600
    tibble::tibble(sample = nm, n = stats::nobs(models[[nm]]),
                   median_gap_h = stats::median(gap_h),
                   pct_gap_1h = 100 * mean(gap_h < 1),
                   lag1 = residual_lag1(models[[nm]], times[[nm]]),
                   dw = durbin_watson(models[[nm]], times[[nm]]))
  })
  dplyr::bind_rows(out)
}

# -----------------------------------------------------------------------------
# Efecto de la época del año
# -----------------------------------------------------------------------------

#' Potencia estimada por estación a una velocidad de giro fija
#'
#' Temperatura en su mediana; RPM en `rpm` (por defecto, la de referencia).
#' Devuelve la mediana estimada de la potencia (W) con su intervalo de
#' confianza al 95 % (prediction_intervals(), escala logarítmica -> W).
season_profile <- function(model, data, rpm = NULL,
                           settings = agreed_settings()) {
  grid <- data.frame(season = factor(levels(data$season),
                                     levels = levels(data$season)))
  grid$rpm <- if (is.null(rpm)) settings$reference_rpm else rpm
  grid$log_rpm_c <- log(grid$rpm / settings$reference_rpm)
  grid$temp_nacelle_c <- stats::median(data$temp_nacelle_c, na.rm = TRUE)
  prediction_intervals(model, grid, "confidence")
}

#' Curva de potencia del modelo acordado con bandas de confianza y predicción
#'
#' Igual que power_curve_grid() (07_inference.R), pero con la estación en su
#' nivel de referencia (verano), que es la predictora de época de este modelo.
#' Devuelve las mismas columnas, para usar plot_power_curve_fit().
agreed_curve_grid <- function(model, data, n = 100,
                              settings = agreed_settings()) {
  r <- stats::quantile(data$rpm, c(0.01, 0.99), names = FALSE)
  grid <- data.frame(rpm = seq(r[1], r[2], length.out = n))
  grid$log_rpm_c <- log(grid$rpm / settings$reference_rpm)
  grid$temp_nacelle_c <- stats::median(data$temp_nacelle_c, na.rm = TRUE)
  grid$season <- factor(levels(data$season)[1], levels = levels(data$season))
  conf <- prediction_intervals(model, grid, "confidence")
  pred <- prediction_intervals(model, grid, "prediction")
  tibble::tibble(rpm = grid$rpm, fit_w = conf$fit_w,
                 conf_lower_w = conf$lower_w, conf_upper_w = conf$upper_w,
                 pred_lower_w = pred$lower_w, pred_upper_w = pred$upper_w)
}

# -----------------------------------------------------------------------------
# Tablas y figuras del modelo acordado (estilo común: 10_tables.R, 11_plots.R)
# -----------------------------------------------------------------------------

#' Tabla de dependencia temporal de los residuos por muestra
table_dependence <- function(dep, caption, note = NULL) {
  out <- tibble::tibble(
    Muestra = dep$sample,
    "Intervalos" = fmt_int(dep$n),
    "Separación mediana (h)" = fmt_num(dep$median_gap_h, 1),
    "Pares a menos de 1 h (%)" = fmt_num(dep$pct_gap_1h),
    "Autocorrelación orden 1" = fmt_num(dep$lag1, 2),
    "Durbin-Watson" = fmt_num(dep$dw, 2)
  )
  # Letra algo menor: seis columnas a ancho de texto
  style_report_table(out, caption = caption, note = note,
                     align = c("l", "r", "r", "r", "r", "r"), font_size = 7.5)
}

#' Potencia estimada por estación a igual velocidad de giro
#'
#' @param profile season_profile().
plot_season_profile <- function(profile) {
  pal <- wind_palette()
  ggplot2::ggplot(profile, ggplot2::aes(season, fit_w)) +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = lower_w, ymax = upper_w),
                           width = 0.2, colour = pal[["neutral"]],
                           linewidth = 0.4) +
    ggplot2::geom_point(colour = pal[["primary"]], size = 2) +
    ggplot2::scale_y_continuous(labels = axis_es(accuracy = 1)) +
    ggplot2::labs(x = NULL, y = "Potencia estimada (W)") +
    theme_wind_report()
}
