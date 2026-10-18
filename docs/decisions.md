# Registro de decisiones

Toda decisión que afecte a los datos o al análisis se registra aquí **antes**
de aplicarse. Cada decisión tiene un identificador que aparece en el código
(`R/02_cleaning.R`, `config/config.yml`) y en el informe.

Tipos:

* **T** — corrección técnica: no cambia la población estudiada.
* **I** — decisión de implementación: parámetro técnico sin efecto sustantivo.
* **D-M** — decisión metodológica: cambia la población o el modelo. Requiere
  acuerdo del grupo.

Estados: ✅ aplicada · 🟡 provisional · ⏳ pendiente del grupo.

---

## Correcciones técnicas (aplicadas en `clean_technical()`)

| Id. | Decisión | Justificación | Efecto en 2017 | Estado |
|---|---|---|---|---|
| T1 | Normalizar el separador de milisegundos de `Log Time` (`,` → `.`) y parsear con el formato documentado | El diccionario fija el formato `YYYY:MM:DD:HH:MM:SS.SSS`; en los datos aparece también `,` | 12 298 marcas con `,`; 0 no parseables | ✅ María |
| T1b | Tratar `Log Time` como hora de reloj local sin zona horaria | El diccionario dice "Local time" pero no da zona ni reglas de horario de verano; aplicarlas sería suponer | — | ✅ María |
| T2 | Eliminar filas duplicadas exactas (todas las columnas originales iguales) | Un duplicado exacto no aporta información | 0 filas | ✅ María |
| T3 | Ordenar por `log_time` | Necesario para huecos, diferencias de contadores y ACF | ya ordenado | ✅ María |
| T4 | Recortar espacios en los nombres de columna (`" T1"` → `T1`) y renombrar con `scada_schema()` | Nombres consistentes entre años | 1 columna | ✅ María |

## Decisiones de implementación

| Id. | Decisión | Justificación | Estado |
|---|---|---|---|
| I1 | Umbral de hueco temporal: 180 s (`config.yml`) | 1 registro/min documentado; el 95 % de los intervalos observados está entre 50 y 70 s | ✅ María |
| I2 | Variable "casi constante": valor modal ≥ 99 % | Convención descriptiva | ✅ María |
| I3 | Correlaciones exploratorias de Spearman | Relaciones monótonas no lineales y muchos ceros en la respuesta | ✅ María |
| I4 | Sin capa `data/interim/` persistente | La lectura y combinación de años es rápida (segundos); guardarla duplicaría datos sin aportar trazabilidad | ✅ María |
| I5 | `Log Time` como referencia temporal; `Inv Time` solo para comprobación | `Log Time − Inv Time` ≈ +4,99 h, incompatible con la diferencia esperada local–GMT; no se interpreta | ✅ María |
| I6 | Los incrementos negativos del contador de energía se conservan | Ocultarlos sesgaría los resúmenes de energía; su causa no está documentada | ✅ María |
| I7 | Definición de hueco: intervalo > 180 s entre registros consecutivos (todos los años combinados y ordenados por `Log Time`); duración = intervalo completo (incluye el minuto nominal); un hueco que cruza el cambio de año se cuenta una vez y se asigna al año del registro posterior | Definición explícita y verificable. `gap_accounting()` comprueba que periodo total = intervalos normales + huecos (error 0 s) y ofrece también la duración neta (intervalo − 60 s) | ✅ María |
| I8 | Potencia nominal de referencia 1.800 W (`config.yml: reference.rated_power_w`) | Dato de los autores (ficha de Zenodo y Bassi et al., 2023), **no** del diccionario. Solo se usa como umbral descriptivo, nunca como filtro | ✅ María |
| I10 | Estilo único: `theme_wind_report()` + `wind_palette()` (3 colores aptos para daltonismo y escala de grises) en todas las figuras; `style_report_table()` (booktabs, notas al pie, alineación automática) en todas las tablas | Coherencia visual entre las partes de los tres autores | ✅ María |
| I11 | Figuras flotantes (`!htb`) que no cruzan secciones (`placeins`); tablas fijas | Evita páginas medio vacías sin desordenar las secciones | ✅ María |
| I9 | Los captions de las tablas se escriben en texto plano y se escapan con `escape_caption()` | kable no escapa los captions; evita LaTeX visible o errores de compilación | ✅ María |

---

## Decisiones metodológicas (requieren acuerdo del grupo)

Propuesta aplicada por Álvaro (11/10/2026) para poder ajustar el modelo,
**pendiente de confirmación del grupo** (estado 🟡). `data/final` = periodo
2018–2022 por minuto; población y agregación del modelo en `R/06_modeling.R`.

### D-M1 · Variable respuesta — 🟡 provisional (aplicada)
* Opciones: `Power out` ("Power output of the inverter (W)") o `Power reg`
  ("Power from generator (W)").
* Evidencia: asociación lineal prácticamente perfecta (Pearson 0,9991 con
  2017–2022; 0,9995 en 2017). La correlación de Spearman, sobre las mismas
  observaciones, es menor (≈ 0,97), en parte por los registros en que una de
  las dos es nula y la otra no. Una correlación tan alta **no garantiza** resultados
  idénticos: hay discrepancias aisladas grandes. Detalle en
  `outputs/tables/response_out_vs_reg_*.csv` (resumen, por estado y casos
  extremos).
* Aplicado: `Power out`, por su interpretación como potencia entregada a la
  red. Comprobación: el modelo principal reajustado con `Power reg`
  (`fit_all_models()`, columna "Power reg" de `model_sensitivity.csv`) da
  coeficientes muy parecidos (elasticidad a 140 rpm 4,74 frente a 4,91; mismos
  signos y órdenes de magnitud en temperatura, año y franja).
* Parámetro: `analysis.response`.

### D-M2 · Periodo de estudio — 🟡 provisional (aplicada)
* 2017 solo cubre 29/11–31/12 (30 días con datos; cobertura 66 %).
* Aplicado: **2018-01-01 a 2022-12-31** (cinco años completos). Con el año
  como factor del modelo, 2017 sería un nivel estimado con un mes de
  verano austral: su coeficiente mezclaría año y estación. El EDA
  (secciones 2-3) sigue usando todos los registros (`data/processed`).
* Parámetros: `analysis.period_start`, `analysis.period_end`.

### D-M3 · Estados operativos incluidos — 🟡 provisional (aplicada)
* Los códigos **no tienen significado documentado**. Empíricamente, con
  `Turbine status` = 0 hay producción en el 99 % de los registros y con 1, 3,
  33, 35 nunca.
* Aplicado: **sin filtro por estado** (`turbine_status_keep: null`). Al
  modelizar solo la producción (D-M6) los estados sin producción quedan
  fuera sin necesidad de interpretar los códigos. Comprobación: el modelo se
  reajusta con los intervalos cuyos minutos tienen todos `turbine_status` = 0
  (columna "Estado 0" de `model_sensitivity.csv`): mismas conclusiones
  cualitativas.
* Parámetro: `analysis.turbine_status_keep`.

### D-M4 · Agregación temporal — 🟡 provisional (aplicada)
* Aplicado: **intervalos de 10 min** con al menos **8 minutos con
  producción**. Es la resolución habitual de las curvas de potencia (norma
  IEC 61400-12-1). Con medias horarias y el mismo criterio (≥ 48 de 60 min)
  solo quedarían 4.863 horas, frente a 37.747 intervalos de 10 min.
* `data/final` se mantiene por minuto (`analysis.aggregation: none`): la
  agregación del modelo se hace en `prepare_conditional_data()`
  (`R/06_modeling.R`) DESPUÉS de seleccionar los minutos con producción;
  agregar antes mezclaría minutos con y sin producción en la media.
* Parámetros: `analysis.model.interval`, `analysis.model.min_producing_minutes`.

### D-M5 · Dependencia temporal — 🟡 provisional (aplicada)
* ACF de la serie de medias horarias: 0,76 a 1 h y máximo local de 0,39 a
  24 h (2017–2022), compatible con una componente diaria.
* El modelo lineal supone errores independientes: con autocorrelación positiva
  no tratada, los errores estándar tienden a estar subestimados.
* Aplicado, con herramientas del curso (sin ARIMA): (1) agregación (D-M4);
  (2) **análisis de sensibilidad con una submuestra espaciada**
  (`thin_sample()`, un intervalo cada **6 h**), en la que la autocorrelación
  de orden 1 de los residuos es despreciable (0,04 frente a 0,23 en la
  muestra completa); (3) la limitación se declara en el informe. El
  diagnóstico formal (Durbin-Watson, ACF de residuos) es de Daniel.
* Parámetro: `analysis.model.thinning_hours`.

### D-M6 · Población de análisis — 🟡 provisional (aplicada) **DECISIÓN CENTRAL**
* `Power out` = 0 en el 67,7 % de los registros (2017–2022) y los valores
  positivos son muy asimétricos (distribución MARGINAL).
* Aplicado: **modelo en dos partes**.
  1. Parte principal (lm): potencia **condicionada a producción**, en
     intervalos de producción sostenida (D-M4). No requiere interpretar los
     códigos de estado. En 2018–2022 no hay ningún minuto con P > 0 y
     RPM = 0 (mínimo 59 rpm), de modo que log(RPM) siempre está definido.
  2. Parte complementaria (glm binomial): probabilidad de que un intervalo
     casi completo (≥ 8 registros) tenga producción en al menos la mitad de
     sus minutos, con predictoras externas a la turbina (temperatura,
     estación, franja del día, año). RPM y estados se excluyen: determinan la
     producción casi por definición (separación).
* Parámetros: `analysis.model.min_records`, `analysis.model.production_share`.

### D-M7 · Tipo de modelo y forma funcional — ✅ Álvaro
* Relación potencia–RPM convexa; colinealidad fuerte entre RPM,
  `Windspeed (ref)`, `Voltage In` y `min v from rpm`.
* Aplicado: `lm(log P ~ l + l^2 + temperatura + año + franja)`, con
  l = log(RPM / 140) (140 rpm = mediana de los intervalos del modelo).
  * El logaritmo convierte la ley potencial P = a·RPM^b en una recta y hace
    los efectos multiplicativos (% de potencia), como en el ejemplo de Forbes
    del Tema 6.
  * El término cuadrático es necesario: con solo l los residuos medios por
    decil de RPM dibujan una U invertida (−0,13 / +0,06 / −0,13) que
    desaparece con l^2; R² pasa de 0,886 a 0,899. La elasticidad
    dlog P / dlog RPM pasa a depender de la velocidad de giro.
  * Centrar en 140 rpm hace que el coeficiente de l sea la elasticidad a esa
    velocidad y reduce la correlación entre l y l^2.
  * Solo RPM entre las variables del rotor; nunca variables consecuencia de
    la respuesta.
  * Alternativa evaluada: `poly(rpm, 3)` en W (R² 0,947 en W, no comparable
    con el R² en log); se descarta por interpretación y porque su
    variabilidad residual crece con la potencia (de 11 a 42 W entre deciles
    de RPM).
* Modelos anidados m1–m5 ajustados sobre las mismas filas
  (`fit_candidate_models()`), entregados a 09_model_selection.R.

### D-M8 · Pregunta de investigación — 🟡 provisional (propuesta de Álvaro)
* ¿Cómo depende la potencia entregada por la turbina, cuando produce, de su
  velocidad de giro? Y, a igual velocidad de giro, ¿difiere según la
  temperatura en la góndola o el año de operación? Complemento: ¿de qué
  depende la probabilidad de que la turbina esté produciendo?
* Combina P1, P2, P3 y P4 de `docs/methodology_notes.md`.

---

## Cuestiones pendientes de interpretación (no son decisiones aún)

### R1 · Registros con potencia superior a la nominal — ⏳
* Con 2017–2022 el máximo de `Power out` es 3.713 W, frente a 1.800 W
  nominales (dato de los autores) y a 2.400 W de `Power max` ("Inverter
  maximum power production" según el diccionario).
* La documentación proporcionada (diccionario y enunciado) **no explica**
  estos valores. No se eliminan ni se interpretan como error.
* Detalle reproducible (`high_power_check()`, ficheros
  `outputs/tables/response_high_power_*.csv`): nº de registros, episodios y
  su duración, años y meses, estados asociados, perfil de RPM y otras
  variables frente a registros con producción normal, y registros por encima
  de `Power max`.
* Entrada para Daniel: revisar si estos registros son influyentes en el
  modelo antes de cualquier decisión.

### R2 · Discrepancias Power out − Power reg — ⏳
* Ver D-M1 y `outputs/tables/response_out_vs_reg_*.csv`.

---

## Historial

| Id. | Cambio | Quién |
|---|---|---|
| T1–T4, I1–I6 | Implementación inicial | María |
| D-M1 | Respuesta provisional `power_out_w` | María (pendiente de confirmar) |
| I10–I11, D-M1, D-M6 | Revisión final: estilo único, Pearson y Spearman etiquetados, distribución marginal frente a condicional | María |
| I7–I9, D-M1, D-M6, R1, R2 | Segunda revisión: definición y verificación de huecos, comprobaciones de la respuesta, población de análisis como decisión central | María |
| D-M1–D-M8 | Propuesta aplicada para el modelo: periodo 2018–2022, sin filtro de estados, intervalos de 10 min de producción sostenida, modelo en dos partes, log-log con término cuadrático, sensibilidad con submuestra espaciada | Álvaro |
