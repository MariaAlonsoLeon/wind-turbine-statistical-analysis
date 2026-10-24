# =============================================================================
# 06_modeling.R — Formulación y ajuste del modelo
# -----------------------------------------------------------------------------
# Responsable: ÁLVARO                                  ESTADO: implementado
#
#
# Decisiones aplicadas (justificación en docs/decisions.md):
#   D-M1 Respuesta: power_out_w.
#   D-M4 Intervalos de 10 min con al menos 8 minutos con producción.
#   D-M6 Población: potencia condicionada a producción (P > 0).
#   D-M7 Modelo log-log con curvatura: log(P) ~ log(RPM/140) + su cuadrado
#        + temperatura + año + franja del día.
#   D-M8 Pregunta: curva de potencia y efecto parcial de temperatura y año.
#
# Modelos del Tema 6: lm() con transformación logarítmica y factores, y
# glm(binomial) para la probabilidad de producción (modelo en dos partes).
# =============================================================================

#' Error estándar para funciones aún no implementadas
not_implemented <- function(owner, what) {
  stop(sprintf("%s: pendiente de implementación por %s.", what, owner),
       call. = FALSE)
}

# -----------------------------------------------------------------------------
# Parámetros del modelo (config.yml -> analysis$model)
# -----------------------------------------------------------------------------

#' Parámetros del modelo con valores por defecto
#'
#' Los valores de config.yml prevalecen y los de aquí solo evitan errores si
#' falta alguna clave (p. ej. en los tests).
model_settings <- function(config = load_config()) {
  defaults <- list(interval = "10min", min_producing_minutes = 8,
                   min_records = 8, production_share = 0.5,
                   reference_year = 2018, reference_rpm = 140,
                   thinning_hours = 6)
  user <- config$analysis$model
  if (is.null(user)) return(defaults)
  utils::modifyList(defaults, user)
}

#' Anchura en segundos de un intervalo de agregación
interval_seconds <- function(interval = c("10min", "hour")) {
  interval <- match.arg(interval)
  c("10min" = 600, hour = 3600)[[interval]]
}

#' Inicio del intervalo regular al que pertenece cada marca temporal
interval_start <- function(log_time, width) {
  as.POSIXct(floor(as.numeric(log_time) / width) * width,
             origin = "1970-01-01", tz = "UTC")
}

#' Franja del día (4 niveles de 6 horas) a partir de la hora de reloj
#' Se usa una franja y no la hora (24 niveles) para que el efecto del ciclo
#' diario se resuma en pocos coeficientes interpretables.
day_period <- function(hour) {
  factor(cut(hour, breaks = c(0, 6, 12, 18, 24), right = FALSE,
             labels = c("0-6 h", "6-12 h", "12-18 h", "18-24 h")),
         levels = c("0-6 h", "6-12 h", "12-18 h", "18-24 h"))
}

#' Estación del año en el hemisferio sur
season_south <- function(month) {
  s <- c("Verano", "Verano", "Otoño", "Otoño", "Otoño", "Invierno",
         "Invierno", "Invierno", "Primavera", "Primavera", "Primavera",
         "Verano")
  factor(s[month], levels = c("Verano", "Otoño", "Invierno", "Primavera"))
}

#' Año como factor con un nivel de referencia explícito
year_factor <- function(year, reference_year = 2018) {
  lv <- sort(unique(year))
  if (reference_year %in% lv) lv <- c(reference_year, setdiff(lv, reference_year))
  factor(year, levels = lv)
}

# -----------------------------------------------------------------------------
# Conjuntos del modelo
# -----------------------------------------------------------------------------

#' Conjunto del modelo condicional: intervalos de producción sostenida
#'
#' 1. Selecciona los minutos con potencia positiva (D-M6) y velocidad de giro
#'    positiva (necesaria para log(RPM), en 2018-2022 no se excluye ninguno).
#' 2. Agrega en intervalos regulares (D-M4): medias de potencia, RPM y
#'    temperatura. Proporción de minutos con `turbine_status` = 0.
#' 3. Conserva los intervalos con al menos `min_producing_minutes` minutos con
#'    producción, de modo que la media describe producción sostenida y no
#'    arranques o paradas.
#' 4. Añade las variables del modelo (logaritmos, año, franja del día).
#'
#' @return tibble con atributo "preparation_log" (filas en cada paso).
prepare_conditional_data <- function(data, settings = model_settings(),
                                     response = "power_out_w") {
  assert_columns(data, c("log_time", response, "rpm", "temp_nacelle_c",
                         "turbine_status"), "prepare_conditional_data")
  width <- interval_seconds(settings$interval)
  n0 <- nrow(data)
  producing <- data[!is.na(data[[response]]) & data[[response]] > 0, ]
  n1 <- nrow(producing)
  producing <- producing[!is.na(producing$rpm) & producing$rpm > 0, ]
  n2 <- nrow(producing)
  producing$interval_start <- interval_start(producing$log_time, width)

  agg <- dplyr::summarise(
    dplyr::group_by(producing, interval_start),
    n_records = dplyr::n(),
    !!response := mean(.data[[response]]),
    rpm = mean(rpm),
    temp_nacelle_c = mean(temp_nacelle_c, na.rm = TRUE),
    status0_share = mean(turbine_status == 0),
    .groups = "drop"
  )
  n3 <- nrow(agg)
  agg <- agg[agg$n_records >= settings$min_producing_minutes, ]
  agg <- agg[!is.na(agg$temp_nacelle_c), ]
  n4 <- nrow(agg)

  out <- agg |>
    dplyr::rename(log_time = interval_start) |>
    add_time_features() |>
    dplyr::mutate(
      log_power = log(.data[[response]]),
      log_rpm = log(rpm),
      log_rpm_c = log(rpm / settings$reference_rpm),
      year_f = year_factor(year, settings$reference_year),
      day_period = day_period(hour)
    )
  attr(out, "preparation_log") <- tibble::tibble(
    step = c("Registros de partida", "Potencia positiva (D-M6)",
             "Velocidad de giro positiva", "Intervalos agregados (D-M4)",
             "Intervalos de producción sostenida"),
    unit = c("minutos", "minutos", "minutos", "intervalos", "intervalos"),
    n = c(n0, n1, n2, n3, n4)
  )
  out
}

#' Conjunto del modelo de probabilidad de producción
#'
#' Todos los minutos se agregan en intervalos regulares. Se conservan los
#' intervalos casi completos (`min_records` registros) y la respuesta binaria
#' indica si la turbina produjo en al menos `production_share` de sus minutos.
#' Solo se guardan predictoras externas a la turbina (tiempo y temperatura):
#' RPM y estados determinan la producción casi por definición.
prepare_production_data <- function(data, settings = model_settings(),
                                    response = "power_out_w") {
  assert_columns(data, c("log_time", response, "temp_nacelle_c"),
                 "prepare_production_data")
  width <- interval_seconds(settings$interval)
  data$interval_start <- interval_start(data$log_time, width)
  agg <- dplyr::summarise(
    dplyr::group_by(data, interval_start),
    n_records = dplyr::n(),
    n_producing = sum(.data[[response]] > 0, na.rm = TRUE),
    temp_nacelle_c = mean(temp_nacelle_c, na.rm = TRUE),
    .groups = "drop"
  )
  n_all <- nrow(agg)
  agg <- agg[agg$n_records >= settings$min_records & !is.na(agg$temp_nacelle_c), ]
  out <- agg |>
    dplyr::rename(log_time = interval_start) |>
    add_time_features() |>
    dplyr::mutate(
      share_producing = n_producing / n_records,
      produce = as.integer(share_producing >= settings$production_share),
      year_f = year_factor(year, settings$reference_year),
      day_period = day_period(hour),
      season = season_south(month)
    )
  attr(out, "preparation_log") <- tibble::tibble(
    step = c("Intervalos con algún registro", "Intervalos casi completos"),
    unit = "intervalos", n = c(n_all, nrow(out)))
  out
}

#' Subconjunto espaciado en el tiempo
#'
#' Recorre los intervalos en orden temporal y conserva uno solo si han pasado
#' al menos `every_hours` horas desde el último conservado. Sirve para
#' comprobar la sensibilidad de la inferencia a la dependencia temporal (D-M5)
#' con observaciones mucho más separadas.
thin_sample <- function(data, every_hours = 6) {
  assert_columns(data, "log_time", "thin_sample")
  data <- data[order(data$log_time), ]
  t <- as.numeric(data$log_time)
  keep <- logical(length(t))
  last <- -Inf
  for (i in seq_along(t)) {
    if (t[i] - last >= every_hours * 3600) {
      keep[i] <- TRUE
      last <- t[i]
    }
  }
  data[keep, ]
}

# -----------------------------------------------------------------------------
# Fórmulas y ajuste
# -----------------------------------------------------------------------------

#' Fórmula(s) candidatas del modelo
#'
#' * m1-m5: modelos ANIDADOS para la potencia condicionada a producción.
#'   m1 es la ley potencial P = a * RPM^b, que el logaritmo convierte en una
#'   recta (b = 3 si el coeficiente de potencia fuera constante). m2 añade el
#'   cuadrado de log(RPM) donde la elasticidad deja de ser constante y pasa a
#'   depender de la velocidad de giro. log(RPM) se centra en `reference_rpm`
#'   (mediana, 140 rpm), de modo que su coeficiente es la elasticidad a esa
#'   velocidad. m3 añade la temperatura en la góndola (densidad del aire),
#'   m4 el año (cambios de rendimiento), m5 la franja del día (ciclo diario).
#' * poly3: alternativa en la escala original con un polinomio cúbico
#'   ortogonal de RPM para comparar la forma funcional.
#' * production: modelo logístico de la probabilidad de producir.
#' Ninguna fórmula usa variables consecuencia de la respuesta (corriente,Power reg) ni windspeed_ref_ms, que se calcula a partir de las RPM.

#' @param response Variable respuesta original (se usa su logaritmo).
#' @return lista nombrada de objetos `formula`.
model_formulas <- function(response = load_config()$analysis$response) {
  if (!identical(response, "power_out_w") && !identical(response, "power_reg_w")) {
    stop("Respuesta no prevista en model_formulas(): ", response, call. = FALSE)
  }
  list(
    m1 = log_power ~ log_rpm_c,
    m2 = log_power ~ log_rpm_c + I(log_rpm_c^2),
    m3 = log_power ~ log_rpm_c + I(log_rpm_c^2) + temp_nacelle_c,
    m4 = log_power ~ log_rpm_c + I(log_rpm_c^2) + temp_nacelle_c + year_f,
    m5 = log_power ~ log_rpm_c + I(log_rpm_c^2) + temp_nacelle_c + year_f +
      day_period,
    poly3 = stats::as.formula(paste(response, "~ poly(rpm, 3)")),
    production = produce ~ temp_nacelle_c + season + day_period + year_f
  )
}

#' Ajusta el modelo principal
#' @param data Conjunto del modelo (prepare_conditional_data() o
#'   prepare_production_data()).
#' @param formula Fórmula elegida (de model_formulas()). Por defecto m5.
#' @param family NULL para lm(); un objeto family para glm().
#' @return objeto "lm" o "glm".
fit_main_model <- function(data, formula = NULL, family = NULL) {
  if (is.null(formula)) formula <- model_formulas()$m5
  formula <- stats::as.formula(formula)
  assert_columns(data, all.vars(formula), "fit_main_model")
  if (is.null(family)) {
    fit <- stats::lm(formula, data = data)
  } else {
    fit <- stats::glm(formula, family = family, data = data)
  }
  # Fórmula explícita en la llamada: así print()/summary() la muestran y
  # update() funciona sin depender del entorno en que se ajustó.
  fit$call$formula <- formula
  fit
}

#' Filas completas en todas las variables de un conjunto de fórmulas
common_rows <- function(data, formulas) {
  vars <- unique(unlist(lapply(formulas, all.vars)))
  assert_columns(data, vars, "common_rows")
  stats::complete.cases(as.data.frame(data)[vars])
}

#' Ajusta varios modelos candidatos con los mismos datos
#'
#' @param data Conjunto del modelo.
#' @param formulas Lista nombrada de fórmulas.
#' @return lista nombrada de modelos ajustados sobre las MISMAS filas (para
#'   que sean comparables en 09_model_selection.R). Atributo "n".
fit_candidate_models <- function(data, formulas, family = NULL) {
  if (is.null(names(formulas)) || any(names(formulas) == "")) {
    stop("fit_candidate_models(): las fórmulas deben tener nombre.",
         call. = FALSE)
  }
  rows <- common_rows(data, formulas)
  data <- data[rows, ]
  models <- lapply(formulas, function(f) fit_main_model(data, f, family))
  attr(models, "n") <- nrow(data)
  models
}

#' Ajusta el modelo logístico de la probabilidad de producción
fit_production_model <- function(data, formula = model_formulas()$production) {
  fit_main_model(data, formula, family = stats::binomial(link = "logit"))
}

# -----------------------------------------------------------------------------
# Ajuste completo (lo ejecuta scripts/03_fit_models.R)
# -----------------------------------------------------------------------------

#' Prepara los datos y ajusta todos los modelos del análisis
#'
#' * candidates: m1-m5 sobre las mismas filas (para 09_model_selection.R).
#' * main: m5, modelo principal.
#' * poly3: forma funcional alternativa en W (comparación).
#' * Sensibilidad del modelo principal: submuestra espaciada (D-M5),
#'   intervalos con turbine_status = 0 en todos sus minutos (D-M3) y respuesta
#'   Power reg (D-M1).
#' * production / production_thinned: modelo logístico y su versión espaciada.
#'
#' @param data Conjunto de análisis por minuto (load_analysis_data("final")).
#' @return lista con los conjuntos del modelo y los modelos ajustados.
fit_all_models <- function(data, settings = model_settings(),
                           response = "power_out_w") {
  f <- model_formulas(response)
  cond <- prepare_conditional_data(data, settings, response)
  prod <- prepare_production_data(data, settings, response)
  candidates <- fit_candidate_models(cond, f[paste0("m", 1:5)])
  cond_reg <- prepare_conditional_data(data, settings, "power_reg_w")
  list(
    settings = settings,
    conditional_data = cond,
    production_data = prod,
    candidates = candidates,
    main = candidates$m5,
    poly3 = fit_main_model(cond, f$poly3),
    thinned = fit_main_model(thin_sample(cond, settings$thinning_hours), f$m5),
    status0 = fit_main_model(cond[cond$status0_share == 1, ], f$m5),
    power_reg = fit_main_model(cond_reg, f$m5),
    production = fit_production_model(prod, f$production),
    production_thinned = fit_production_model(
      thin_sample(prod, settings$thinning_hours), f$production)
  )
}

#' Lee los resultados del modelo guardados por scripts/03_fit_models.R
#' Si no existen, ejecuta el script (igual que load_analysis_data() construye los datos), de modo que el informe puede generarse directamente con Knit.
load_model_results <- function(dir = project_path("models_dir"),
                               build_if_missing = TRUE) {
  path <- file.path(dir, "model_results.rds")
  if (!file.exists(path)) {
    if (!build_if_missing) {
      stop("No existe ", path, ". Ejecuta scripts/03_fit_models.R",
           call. = FALSE)
    }
    message("No existe model_results.rds; se ajustan ahora los modelos.")
    source(here::here("scripts", "03_fit_models.R"), local = new.env())
  }
  readRDS(path)
}

#' Guarda un modelo ajustado en outputs/models (utilidad, ya operativa)
save_model <- function(model, name, dir = project_path("models_dir")) {
  ensure_dir(dir)
  path <- file.path(dir, paste0(name, ".rds"))
  saveRDS(model, path)
  invisible(path)
}
