# =============================================================================
# 05_eda.R — Análisis exploratorio (cálculos)
# -----------------------------------------------------------------------------
# Responsable: María
# Todas las funciones devuelven tibbles; el formato de tablas está en
# 10_tables.R y los gráficos en 11_plots.R. Ninguna filtra datos: cuando un
# resumen se calcula sobre un subconjunto, el subconjunto es un argumento
# explícito y aparece en el nombre de la columna (p. ej. *_if_producing).
# =============================================================================

#' Resumen numérico de variables continuas
numeric_summary <- function(data, vars) {
  rows <- lapply(vars, function(v) {
    x <- data[[v]]
    tibble::tibble(
      variable = v,
      n = sum(!is.na(x)),
      mean = mean(x, na.rm = TRUE),
      sd = stats::sd(x, na.rm = TRUE),
      min = min(x, na.rm = TRUE),
      q1 = stats::quantile(x, .25, na.rm = TRUE, names = FALSE),
      median = stats::median(x, na.rm = TRUE),
      q3 = stats::quantile(x, .75, na.rm = TRUE, names = FALSE),
      max = max(x, na.rm = TRUE),
      pct_zero = 100 * mean(x == 0, na.rm = TRUE),
      # decimales para mostrar valores observados: 0 si la variable es entera
      digits = if (all(x == round(x), na.rm = TRUE)) 0 else 1
    )
  })
  dplyr::bind_rows(rows)
}

#' Resumen anual de la respuesta y de la cobertura de datos
summarise_by_year <- function(data, response = "power_out_w") {
  y <- rlang::sym(response)
  dplyr::group_by(data, year) |>
    dplyr::summarise(
      n_records = dplyr::n(),
      n_days = dplyr::n_distinct(date),
      pct_producing = 100 * mean(!!y > 0, na.rm = TRUE),
      mean_power = mean(!!y, na.rm = TRUE),
      mean_power_if_producing = mean((!!y)[!!y > 0], na.rm = TRUE),
      p99_power = stats::quantile(!!y, .99, na.rm = TRUE, names = FALSE),
      max_power = max(!!y, na.rm = TRUE),
      net_energy_kwh = sum(energy_increment_wh, na.rm = TRUE) / 1000,
      .groups = "drop"
    )
}

#' Frecuencia de cada código de estado y comportamiento de la respuesta
#'
#' Los códigos NO tienen significado documentado: la tabla es empírica.
summarise_by_status <- function(data, status_var = "turbine_status",
                                response = "power_out_w") {
  s <- rlang::sym(status_var)
  y <- rlang::sym(response)
  dplyr::group_by(data, code = !!s) |>
    dplyr::summarise(
      n = dplyr::n(),
      pct_producing = 100 * mean(!!y > 0, na.rm = TRUE),
      mean_power = mean(!!y, na.rm = TRUE),
      median_power = stats::median(!!y, na.rm = TRUE),
      max_power = max(!!y, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(pct = 100 * n / sum(n), .after = n) |>
    dplyr::arrange(dplyr::desc(n))
}

#' Respuesta condicionada a un predictor discreto (curva de potencia empírica)
summarise_response_by_level <- function(data, predictor = "windspeed_ref_ms",
                                        response = "power_out_w") {
  x <- rlang::sym(predictor)
  y <- rlang::sym(response)
  dplyr::group_by(data, level = !!x) |>
    dplyr::summarise(
      n = dplyr::n(),
      mean = mean(!!y, na.rm = TRUE),
      sd = stats::sd(!!y, na.rm = TRUE),
      q1 = stats::quantile(!!y, .25, na.rm = TRUE, names = FALSE),
      median = stats::median(!!y, na.rm = TRUE),
      q3 = stats::quantile(!!y, .75, na.rm = TRUE, names = FALSE),
      .groups = "drop"
    )
}

#' Matriz de correlaciones (Spearman por defecto: relaciones monótonas no
#' lineales y muchos ceros en la respuesta)
correlation_matrix <- function(data, vars, method = "spearman") {
  stats::cor(data[vars], use = "pairwise.complete.obs", method = method)
}

#' Correlación de cada variable con la respuesta, ordenada por magnitud
#'
#' Los códigos de estado son nominales: se excluyen (NA) aunque sean números.
response_correlations <- function(data, vars, response = "power_out_w",
                                  method = "spearman") {
  vars <- setdiff(vars, c(response, status_columns()))
  r <- vapply(vars, function(v) {
    x <- data[[v]]
    if (stats::sd(x, na.rm = TRUE) %in% c(0, NA)) return(NA_real_)
    stats::cor(x, data[[response]], use = "pairwise.complete.obs",
               method = method)
  }, numeric(1))
  tibble::tibble(variable = vars, correlation = r) |>
    dplyr::arrange(dplyr::desc(abs(correlation)))
}

#' Observaciones extremas por variable (regla de Tukey con k*IQR)
#'
#' Solo cuenta y describe; NO elimina nada. Con distribuciones con muchos
#' ceros (la potencia) la regla es poco informativa y se indica.
count_extreme_values <- function(data, vars, k = 3) {
  rows <- lapply(vars, function(v) {
    x <- data[[v]]
    q <- stats::quantile(x, c(.25, .75), na.rm = TRUE, names = FALSE)
    iqr <- diff(q)
    lo <- q[1] - k * iqr
    hi <- q[2] + k * iqr
    tibble::tibble(variable = v, lower_fence = lo, upper_fence = hi,
                   n_below = sum(x < lo, na.rm = TRUE),
                   n_above = sum(x > hi, na.rm = TRUE),
                   iqr_zero = iqr == 0)
  })
  dplyr::bind_rows(rows)
}

#' Resumen diario: cobertura (registros/1440) y potencia media
summarise_daily <- function(data, response = "power_out_w") {
  y <- rlang::sym(response)
  dplyr::group_by(data, date) |>
    dplyr::summarise(n_records = dplyr::n(),
                     coverage_pct = 100 * n_records / 1440,
                     mean_power = mean(!!y, na.rm = TRUE),
                     .groups = "drop")
}

#' Perfil medio por hora del día (hora local de reloj)
summarise_diurnal_profile <- function(data, response = "power_out_w") {
  y <- rlang::sym(response)
  dplyr::group_by(data, hour) |>
    dplyr::summarise(n = dplyr::n(),
                     mean_power = mean(!!y, na.rm = TRUE),
                     pct_producing = 100 * mean(!!y > 0, na.rm = TRUE),
                     .groups = "drop")
}

#' Serie temporal REGULAR de medias por intervalo (p. ej. horaria)
#'
#' Cómo se construye (necesario para que la ACF tenga sentido con huecos):
#' 1. Cada registro se asigna al intervalo [k*w, (k+1)*w) que contiene su
#'    log_time (hora de reloj; w = `width_s` segundos, 3600 = hora).
#' 2. El valor del intervalo es la MEDIA de la respuesta de todos sus
#'    registros (con ~60 registros/h, la media horaria de la potencia).
#' 3. Se crea una rejilla completa de intervalos entre el primero y el último
#'    observados. Los intervalos sin ningún registro quedan como NA: NO se
#'    imputan ni se eliminan, para no unir como "consecutivas" horas que en
#'    realidad están separadas por un hueco.
#'
#' @param width_s Anchura del intervalo en segundos (60 = minuto).
#' @return tibble con time (inicio del intervalo) y value (media o NA).
regular_series <- function(data, response = "power_out_w", width_s = 3600) {
  t_bin <- floor(as.numeric(data$log_time) / width_s) * width_s
  agg <- tapply(data[[response]], t_bin, mean, na.rm = TRUE)
  grid <- seq(min(t_bin, na.rm = TRUE), max(t_bin, na.rm = TRUE),
              by = width_s)
  tibble::tibble(time = as.POSIXct(grid, origin = "1970-01-01", tz = "UTC"),
                 value = unname(agg[as.character(grid)]))
}

#' Autocorrelación muestral de una serie regular con huecos
#'
#' Cómo se calcula: stats::acf() con na.action = na.pass. Para cada retardo k
#' se usan solo los pares (t, t + k) en los que ambos intervalos tienen dato;
#' la media y la varianza se estiman con todos los valores no perdidos.
#' Bandas aproximadas al 95 %: +-1,96 / sqrt(n), con n = nº de intervalos con
#' dato (referencia habitual bajo ruido blanco; con muchos huecos es solo
#' orientativa).
#'
#' Uso DESCRIPTIVO: cuantifica la dependencia temporal; no la modeliza ni la
#' corrige (decisión pendiente D-M5; sin ARIMA en este trabajo).
acf_table <- function(series, lag_max = 48) {
  a <- stats::acf(series$value, lag.max = lag_max, na.action = stats::na.pass,
                  plot = FALSE)
  n_eff <- sum(!is.na(series$value))
  tibble::tibble(lag = as.numeric(a$lag), acf = as.numeric(a$acf),
                 ci = stats::qnorm(.975) / sqrt(n_eff))
}

#' Papel analítico PRELIMINAR de cada variable, deducido solo del diccionario
#'
#' Es una propuesta para discusión del grupo, no una selección de variables.
variable_roles <- function() {
  tibble::tribble(
    ~name, ~role, ~note,
    "power_out_w", "Respuesta (provisional, D-M1)", "Potencia de salida del inversor",
    "power_reg_w", "Respuesta alternativa", "Misma magnitud física que la respuesta; no usar como predictora",
    "current_out_a", "Consecuencia de la respuesta", "Corriente de salida: circularidad si se usa como predictora",
    "current_amplitude", "Consecuencia de la respuesta", "Proporcional a la corriente de salida",
    "energy_wh_cum", "Consecuencia de la respuesta", "Contador acumulado de energía",
    "rpm", "Candidata principal", "Velocidad de giro del rotor",
    "windspeed_ref_ms", "Candidata con cautela", "Entera y calculada a partir de RPM: no es medida independiente del viento",
    "voltage_in_v", "Candidata con cautela", "Tensión generada por el rotor: ligada a RPM (colinealidad)",
    "min_v_from_rpm_v", "Candidata con cautela", "Función de RPM según el diccionario",
    "target_tsr", "Consigna de control", "Significado operativo no documentado",
    "ramp_rpm", "Consigna de control", "Unidad no documentada",
    "boost_pulsewidth", "Control del inversor", "Posible circularidad con la potencia",
    "max_bpw", "Control del inversor", "Unidad no documentada",
    "voltage_dc_bus_v", "Candidata secundaria", "Estado del inversor",
    "voltage_l1_v", "Candidata secundaria", "Condición de la red",
    "voltage_l2_v", "Candidata secundaria", "Condición de la red",
    "line_freq_hz", "Candidata secundaria", "Condición de la red",
    "inverter_freq_hz", "Candidata secundaria", "Estado del inversor",
    "line_resistance_ohm", "Candidata secundaria", "Impedancia de línea",
    "temp_heatsink1_c", "Candidata con cautela", "Disipador del inversor: puede reflejar la propia producción",
    "temp_heatsink2_c", "Candidata con cautela", "Disipador del inversor: puede reflejar la propia producción",
    "temp_nacelle_c", "Candidata secundaria", "Temperatura en la góndola",
    "turbine_status", "Factor de estratificación", "Códigos sin significado documentado",
    "grid_status", "Factor de estratificación", "Códigos sin significado documentado",
    "system_status", "Factor de estratificación", "Códigos sin significado documentado",
    "slave_status", "Factor de estratificación", "Descripción idéntica a System status en el diccionario",
    "event_status", "Registro de eventos", "Códigos sin significado documentado",
    "event_value", "Registro de eventos", "Códigos sin significado documentado",
    "last_event_code", "Registro de eventos", "Códigos sin significado documentado",
    "event_count_cum", "Registro de eventos", "Contador acumulado",
    "timer", "Registro de eventos", "Cuenta atrás tras un evento; unidad no documentada",
    "access_status", "No informativa", "Casi constante",
    "power_max_w", "No informativa", "Casi constante (límite del inversor)",
    "voltage_rise_v", "No informativa", "Constante en los datos",
    "serial_number", "No informativa", "Constante documentada",
    "software_rev", "No informativa", "Constante documentada",
    "op_version", "No informativa", "Constante documentada"
  )
}

#' Tabla de variable respuesta y candidatas (metadatos + datos)
#'
#' Combina el esquema (significado, unidad, tipo), el % de perdidos
#' calculado en los datos y el papel preliminar.
candidate_variables <- function(data, response = "power_out_w",
                                schema = scada_schema()) {
  roles <- variable_roles()
  miss <- summarise_missing(data, roles$name)
  rho <- response_correlations(data, roles$name, response)
  roles |>
    dplyr::left_join(schema[c("name", "original", "description", "unit",
                              "type")], by = "name") |>
    dplyr::left_join(miss[c("variable", "pct_missing")],
                     by = c("name" = "variable")) |>
    dplyr::left_join(rho, by = c("name" = "variable")) |>
    dplyr::select(variable = original, name, description, type, unit,
                  pct_missing, spearman_with_response = correlation, role, note)
}

#' Variables de la versión REDUCIDA de la tabla de candidatas (informe)
#'
#' Criterio: respuesta, su alternativa, las variables del rotor y una por
#' grupo físico (temperatura, red) más el estado de turbina. No es una
#' selección de variables para el modelo; la tabla completa está en
#' outputs/tables/eda_candidate_variables.csv.
report_candidate_names <- function() {
  c("power_out_w", "power_reg_w", "rpm", "windspeed_ref_ms", "voltage_in_v",
    "current_out_a", "temp_nacelle_c", "voltage_l1_v", "turbine_status")
}

# -----------------------------------------------------------------------------
# Comprobaciones específicas de la variable respuesta
# -----------------------------------------------------------------------------

#' Valor más frecuente de un vector (para códigos)
mode_code <- function(x) {
  x <- x[!is.na(x)]
  if (!length(x)) return(NA_real_)
  u <- unique(x)
  u[which.max(tabulate(match(x, u)))]
}

#' Compara dos medidas de potencia (por defecto Power out - Power reg)
#'
#' @return lista con `summary` (una fila), `by_status` (por turbine_status)
#'   y `extremes` (los `n_extremes` registros con mayor discrepancia).
compare_power_measures <- function(data, a = "power_out_w", b = "power_reg_w",
                                   n_extremes = 20) {
  # Mismas observaciones para todos los estadísticos (casos completos)
  ok <- stats::complete.cases(data[[a]], data[[b]])
  data <- data[ok, , drop = FALSE]
  d <- data[[a]] - data[[b]]
  ad <- abs(d)
  q <- stats::quantile(d, c(.01, .5, .99), na.rm = TRUE, names = FALSE)
  both_pos <- data[[a]] > 0 | data[[b]] > 0
  summary <- tibble::tibble(
    n = sum(!is.na(d)),
    correlation = stats::cor(data[[a]], data[[b]]),               # Pearson
    spearman = stats::cor(data[[a]], data[[b]], method = "spearman"),
    # registros en que exactamente una de las dos medidas es nula: con tantos
    # empates en 0 afectan a los rangos (Spearman) mucho más que a Pearson
    pct_one_zero = 100 * mean(xor(data[[a]] == 0, data[[b]] == 0)),
    mean_diff = mean(d, na.rm = TRUE),
    sd_diff = stats::sd(d, na.rm = TRUE),
    p01_diff = q[1], median_diff = q[2], p99_diff = q[3],
    min_diff = min(d, na.rm = TRUE), max_diff = max(d, na.rm = TRUE),
    pct_equal = 100 * mean(d == 0, na.rm = TRUE),
    pct_abs_gt_50 = 100 * mean(ad > 50, na.rm = TRUE),
    median_abs_diff_if_producing = stats::median(ad[both_pos], na.rm = TRUE),
    p99_abs_diff_if_producing = stats::quantile(ad[both_pos], .99,
                                                na.rm = TRUE, names = FALSE)
  )
  by_status <- tibble::tibble(code = data$turbine_status, diff = d,
                              abs_diff = ad) |>
    dplyr::group_by(code) |>
    dplyr::summarise(n = dplyr::n(),
                     mean_diff = mean(diff, na.rm = TRUE),
                     median_abs_diff = stats::median(abs_diff, na.rm = TRUE),
                     p99_abs_diff = stats::quantile(abs_diff, .99,
                                                    na.rm = TRUE,
                                                    names = FALSE),
                     max_abs_diff = max(abs_diff, na.rm = TRUE),
                     .groups = "drop") |>
    dplyr::arrange(dplyr::desc(n))
  top <- utils::head(order(ad, decreasing = TRUE), n_extremes)
  extremes <- tibble::tibble(
    log_time = data$log_time[top], power_out_w = data[[a]][top],
    power_reg_w = data[[b]][top], diff = d[top], rpm = data$rpm[top],
    turbine_status = data$turbine_status[top],
    grid_status = data$grid_status[top],
    starts_after_gap = data$starts_after_gap[top]
  )
  list(summary = summary, by_status = by_status, extremes = extremes)
}

#' Identifica episodios: rachas de registros consecutivos que cumplen
#' `flag` dentro del mismo tramo sin huecos (segment_id)
flag_episodes <- function(flag, segment_id) {
  flag[is.na(flag)] <- FALSE
  prev_flag <- c(FALSE, utils::head(flag, -1))
  prev_seg <- c(NA, utils::head(segment_id, -1))
  starts <- flag & (!prev_flag | segment_id != prev_seg)
  ifelse(flag, cumsum(starts), NA_integer_)
}

#' Registros con potencia por encima de un umbral (p. ej. la nominal)
#'
#' No elimina ni interpreta: cuantifica cuántos son, cuándo ocurren, cuánto
#' duran (episodios consecutivos), en qué estados y con qué valores de las
#' demás variables. Compara también con el valor registrado de `Power max`
#' ("Inverter maximum power production" según el diccionario).
#'
#' @param threshold Umbral en W (config.yml: reference$rated_power_w).
#' @return lista de tibbles: summary, by_year, by_month, by_status,
#'   episodes, profile.
high_power_check <- function(data, threshold, response = "power_out_w") {
  y <- data[[response]]
  above <- !is.na(y) & y > threshold
  ep <- flag_episodes(above, data$segment_id)
  d_above <- data[above, , drop = FALSE]
  d_above$episode <- ep[above]

  episodes <- dplyr::group_by(d_above, episode) |>
    dplyr::summarise(start = min(log_time), end = max(log_time),
                     n_records = dplyr::n(),
                     duration_min = as.numeric(difftime(end, start,
                                                        units = "mins")),
                     max_power = max(.data[[response]]),
                     median_rpm = stats::median(rpm),
                     turbine_status = mode_code(turbine_status),
                     .groups = "drop") |>
    dplyr::arrange(dplyr::desc(n_records))

  by_month <- dplyr::count(d_above, month = format(log_time, "%Y-%m"),
                           name = "n_records") |>
    dplyr::arrange(dplyr::desc(n_records))
  top3_share <- if (nrow(by_month)) {
    100 * sum(utils::head(by_month$n_records, 3)) / sum(by_month$n_records)
  } else NA_real_

  by_year <- tibble::tibble(year = data$year, above = above,
                            producing = !is.na(y) & y > 0) |>
    dplyr::group_by(year) |>
    dplyr::summarise(n_above = sum(above),
                     pct_of_records = 100 * mean(above),
                     pct_of_producing = 100 * sum(above) /
                       max(sum(producing), 1),
                     .groups = "drop")

  by_status <- dplyr::bind_rows(lapply(
    c("turbine_status", "grid_status", "system_status"), function(v) {
      dplyr::count(d_above, code = .data[[v]], name = "n") |>
        dplyr::mutate(variable = v, pct = 100 * n / sum(n), .before = 1)
    }))

  profile_vars <- intersect(c(response, "power_reg_w", "rpm",
                              "windspeed_ref_ms", "voltage_in_v",
                              "current_out_a", "voltage_l1_v",
                              "temp_heatsink1_c", "temp_nacelle_c"),
                            names(data))
  group <- ifelse(above, paste0("> ", threshold, " W"),
                  ifelse(!is.na(y) & y > 0, paste0("0 < P <= ", threshold, " W"),
                         NA))
  profile <- dplyr::bind_rows(lapply(profile_vars, function(v) {
    tibble::tibble(variable = v,
                   median_producing = stats::median(data[[v]][which(group == paste0("0 < P <= ", threshold, " W"))], na.rm = TRUE),
                   median_above = stats::median(data[[v]][above], na.rm = TRUE),
                   max_above = if (any(above)) max(data[[v]][above], na.rm = TRUE) else NA_real_)
  }))

  above_pmax <- !is.na(y) & !is.na(data$power_max_w) &
    data$power_max_w > 0 & y > data$power_max_w
  summary <- tibble::tibble(
    threshold_w = threshold,
    n_above = sum(above),
    pct_of_records = 100 * mean(above),
    pct_of_producing = 100 * sum(above) / max(sum(!is.na(y) & y > 0), 1),
    n_episodes = dplyr::n_distinct(stats::na.omit(ep)),
    median_episode_records = if (nrow(episodes)) stats::median(episodes$n_records) else NA_real_,
    max_episode_records = if (nrow(episodes)) max(episodes$n_records) else NA_real_,
    max_power = if (any(above)) max(y[above]) else NA_real_,
    time_of_max = if (any(above)) format(data$log_time[which.max(y)], "%Y-%m-%d %H:%M") else NA_character_,
    years = paste(sort(unique(d_above$year)), collapse = ", "),
    top3_months_share = top3_share,
    top3_months = paste(utils::head(by_month$month, 3), collapse = ", "),
    main_turbine_status = mode_code(d_above$turbine_status),
    pct_main_turbine_status = if (any(above)) 100 * mean(d_above$turbine_status == mode_code(d_above$turbine_status)) else NA_real_,
    n_above_power_max = sum(above_pmax),
    max_power_max_recorded = max(data$power_max_w, na.rm = TRUE)
  )
  list(summary = summary, by_year = by_year, by_month = by_month,
       by_status = by_status, episodes = episodes, profile = profile)
}

#' Resumen mensual con rejilla completa de meses
#'
#' coverage_pct = registros / minutos del mes natural (1 registro/min según
#' el diccionario). Los meses sin registros aparecen con cobertura 0 y
#' potencia media NA. Los meses extremos pueden estar incompletos.
summarise_monthly <- function(data, response = "power_out_w") {
  month_start <- as.Date(format(data$log_time, "%Y-%m-01"))
  agg <- tibble::tibble(month = month_start, y = data[[response]]) |>
    dplyr::group_by(month) |>
    dplyr::summarise(n_records = dplyr::n(),
                     mean_power = mean(y, na.rm = TRUE),
                     pct_producing = 100 * mean(y > 0, na.rm = TRUE),
                     .groups = "drop")
  grid <- tibble::tibble(month = seq(min(month_start), max(month_start),
                                     by = "month"))
  out <- dplyr::left_join(grid, agg, by = "month")
  out$n_records[is.na(out$n_records)] <- 0
  next_month <- seq(min(grid$month), by = "month",
                    length.out = nrow(grid) + 1)[-1]
  minutes <- as.numeric(difftime(next_month, out$month, units = "mins"))
  dplyr::mutate(out, coverage_pct = 100 * n_records / minutes)
}
