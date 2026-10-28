# Datos simulados para los tests del modelo (R/06, R/07)
#
# Se genera una serie por minuto con una ley CONOCIDA a nivel de intervalo de
# 10 min:  log(P) = log(p140) + b * log(RPM / 140) + g * temp + e,
# e ~ N(0, sigma). Dentro de cada intervalo RPM, temperatura y error son
# constantes, de modo que la media del intervalo cumple la ley y los
# estimadores deben recuperar b y g.
# Una fracción de intervalos tiene RPM bajas y potencia nula (turbina parada).
# Con phi > 0 los errores de intervalos consecutivos siguen un AR(1), como la
# dependencia temporal observada en los datos reales.

simulate_scada_minutes <- function(n_intervals = 600, b = 3, g = -0.004,
                                   p140 = 110, sigma = 0.15,
                                   years = c(2018, 2019), share_stopped = 0.3,
                                   phi = 0, spacing_hours = NULL, seed = 1) {
  set.seed(seed)
  # Intervalos repartidos entre los años indicados. Por defecto se reparten a
  # lo largo de todo el año (todas las estaciones y franjas del día); con
  # spacing_hours se fija la separación (p. ej. 1 h para series continuas).
  per_year <- ceiling(n_intervals / length(years))
  if (is.null(spacing_hours)) spacing_hours <- max(1, floor(8760 / per_year))
  starts <- unlist(lapply(years, function(y) {
    as.numeric(as.POSIXct(paste0(y, "-01-01"), tz = "UTC")) +
      (seq_len(per_year) - 1) * spacing_hours * 3600
  }))[seq_len(n_intervals)]
  stopped <- stats::runif(n_intervals) < share_stopped
  rpm_i <- ifelse(stopped, stats::runif(n_intervals, 0, 50),
                  stats::runif(n_intervals, 100, 220))
  temp_i <- stats::runif(n_intervals, 10, 35)
  e_i <- stats::rnorm(n_intervals, 0, sigma)
  if (phi > 0) {
    e_i <- as.numeric(stats::filter(e_i * sqrt(1 - phi^2), phi,
                                    method = "recursive"))
  }
  log_p_i <- log(p140) + b * log(pmax(rpm_i, 1) / 140) + g * temp_i + e_i

  minutes <- 0:9
  d <- data.frame(
    interval = rep(seq_len(n_intervals), each = 10),
    log_time = as.POSIXct(rep(starts, each = 10) + minutes * 60 + 1,
                          origin = "1970-01-01", tz = "UTC")
  )
  d$rpm <- rep(rpm_i, each = 10)
  d$temp_nacelle_c <- rep(temp_i, each = 10)
  d$power_out_w <- ifelse(rep(stopped, each = 10), 0,
                          exp(rep(log_p_i, each = 10)))
  d$power_reg_w <- d$power_out_w * 1.01
  d$turbine_status <- ifelse(d$power_out_w > 0, 0, 1)
  d$interval <- NULL
  tibble::as_tibble(d)
}

#' Ajuste de referencia de los tests: datos simulados -> conjunto del modelo
simulated_model_data <- function(...) {
  settings <- model_settings(list(analysis = list(model = NULL)))
  prepare_conditional_data(simulate_scada_minutes(...), settings)
}
