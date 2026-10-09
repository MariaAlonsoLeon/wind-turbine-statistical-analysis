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

Por defecto `config.yml` **no aplica ningún filtro**: `data/final` es igual a
`data/processed` hasta que el grupo decida.

### D-M1 · Variable respuesta — 🟡 provisional
* Opciones: `Power out` ("Power output of the inverter (W)") o `Power reg`
  ("Power from generator (W)").
* Evidencia: asociación lineal prácticamente perfecta (Pearson 0,9991 con
  2017–2022; 0,9995 en 2017). La correlación de Spearman, sobre las mismas
  observaciones, es menor (≈ 0,97), en parte por los registros en que una de
  las dos es nula y la otra no. Una correlación tan alta **no garantiza** resultados
  idénticos: hay discrepancias aisladas grandes. Detalle en
  `outputs/tables/response_out_vs_reg_*.csv` (resumen, por estado y casos
  extremos).
* Propuesta: `Power out`, por su interpretación como potencia a la salida
  del inversor. Revisar si las discrepancias resultan relevantes (p. ej.
  ajustando el modelo final también con `Power reg` como comprobación).
* Parámetro: `analysis.response`.

### D-M2 · Periodo de estudio — ⏳
* 2017 solo cubre 29/11–31/12 (30 días con datos; cobertura 66 %). ¿Incluirlo, o usar
  años completos?
* Parámetros: `analysis.period_start`, `analysis.period_end`.

### D-M3 · Estados operativos incluidos — ⏳
* Los códigos **no tienen significado documentado**. Empíricamente, con
  `Turbine status` = 0 hay producción en el 99 % de los registros y con 1, 3,
  33, 35 nunca.
* Opciones: (a) no filtrar y modelizar todo; (b) filtrar por estado con una
  justificación externa (manual del fabricante Skystream 3.7); (c) modelizar
  solo registros con potencia > 0 (ver D-M6). Cualquier opción debe
  explicarse en el informe.
* Parámetro: `analysis.turbine_status_keep`.

### D-M4 · Agregación temporal — ⏳
* Opciones: minuto (sin agregar), 10 min, hora. Agregar reduce la
  autocorrelación de alta frecuencia y el tamaño, pero promedia estados.
* Implementado en `aggregate_time()`: medias para continuas, último valor
  para contadores y valor modal para códigos.
* Parámetro: `analysis.aggregation`.

### D-M5 · Dependencia temporal — ⏳
* ACF de la serie de medias horarias: 0,70 a 1 h y máximo local de 0,38 a 24 h (2017), compatible con una componente o periodicidad diaria.
* El modelo lineal supone errores independientes: con autocorrelación positiva
  no tratada, los errores estándar tienden a estar subestimados.
* Opciones a discutir (sin ARIMA, fuera del alcance): agregar (D-M4),
  submuestrear con separación temporal, o mantener el modelo y declarar la
  limitación; contrastarlo con Durbin-Watson en el diagnóstico (Daniel).

### D-M6 · Población de análisis — ⏳ **DECISIÓN CENTRAL antes del ajuste**
* `Power out` = 0 en el 67,7 % de los registros (2017–2022) y los valores
  positivos son muy asimétricos. Esto describe la distribución MARGINAL de la
  respuesta; las hipótesis del modelo lineal se refieren a la distribución de
  los errores CONDICIONADA a las predictoras. Son señales que condicionan la
  población y la especificación, cuya adecuación se evaluará con el ajuste y
  el diagnóstico de residuos.
* Opciones (no decididas):
  1. modelizar toda la potencia observada (ceros incluidos);
  2. modelizar la potencia condicionada a producción positiva (P > 0), y
     declararlo como modelo condicional;
  3. definir una población operacional concreta (por estados, D-M3, o por
     rangos de RPM), con justificación explícita.
  Variante de 1+2: modelo en dos partes (¿produce? → GLM binomial;
  ¿cuánto? → lm).
* Determina la pregunta de investigación (D-M8) y el tipo de modelo (D-M7).

### D-M7 · Tipo de modelo y forma funcional — ⏳ (Álvaro)
* Relación potencia–RPM claramente convexa; colinealidad fuerte entre RPM,
  `Windspeed (ref)`, `Voltage In` y `min v from rpm`.
* `Windspeed (ref)` se calcula a partir de RPM: usar ambas sería redundante.

### D-M8 · Pregunta de investigación — ⏳ (grupo)
* Depende de D-M6. Propuestas en `docs/methodology_notes.md`.

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
