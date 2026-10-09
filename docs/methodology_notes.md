# Notas metodológicas

Documento de trabajo del grupo. Recoge lo observado en el análisis
exploratorio (María) y las cuestiones abiertas para el modelo (Álvaro) y el
diagnóstico (Daniel). Salvo que se indique "2017–2022", las cifras son de
**2017**; las cifras actualizadas de todos los años están en
`outputs/tables/` (`source("scripts/02_run_eda.R")`) y en el propio informe,
que las calcula automáticamente.

Cifras de la ejecución completa 2017–2022 conocidas por el grupo:
1.443.381 registros; `Power out` = 0 en el 67,7 %; correlación
`Power out`–`Power reg` 0,9991; máximo de `Power out` 3.713 W; 6.434 huecos
de más de 3 min que suman 20.560 h; 66.006 descensos en los contadores
acumulados (energía y eventos).

## 1. Hallazgos del análisis exploratorio

1. **Respuesta con exceso de ceros → decisión central sobre la población
   (D-M6).** `Power out` = 0 en el 67,7 % de los registros (2017–2022). Cuando
   es positiva, la distribución es muy asimétrica a la derecha. Es la
   distribución marginal: no invalida por sí sola un modelo lineal (cuyas
   hipótesis son sobre los errores condicionados), pero obliga a decidir
   antes del ajuste si se modeliza toda la potencia, la potencia condicionada
   a P > 0 o una población operacional definida, y a comprobar después la
   adecuación con el diagnóstico.
2. **Relación fuertemente no lineal** con la velocidad de giro: potencia nula o
   muy baja por debajo de unas 100 rpm (no hay producción por debajo de 60 rpm)
   y crecimiento convexo después (curva de
   potencia empírica). Spearman: RPM 0,90, `Windspeed (ref)` 0,91,
   `Voltage In` 0,76.
3. **Colinealidad**: RPM, `Windspeed (ref)`, `Voltage In` y `min v from rpm`
   miden esencialmente lo mismo (Spearman entre 0,83 y 1,00;
   `min v from rpm` es una función monótona exacta de RPM).
   `Windspeed (ref)` se calcula a partir de las RPM según el diccionario: no es
   una medida independiente del viento y su relación con la potencia es en
   parte mecánica, no causal.
4. **Variables que son consecuencia de la respuesta** (`Current out`,
   `current amplitude`, `Power reg`, `Boost pulswidth`, contador de energía):
   no deben usarse como predictoras de la potencia (circularidad).
5. **Códigos de estado**: separan casi perfectamente registros con y sin
   producción (`Turbine status` = 0 → 99 % con producción), pero el
   diccionario no documenta su significado.
6. **Dependencia temporal**: ACF de la potencia media horaria 0,70 a 1 h y
   máximo local de 0,38 a 24 h (2017), compatible con una componente o
   periodicidad diaria (no se atribuye una causa). Los registros próximos en
   el tiempo no son independientes: afecta a la hipótesis de independencia
   del modelo y a la inferencia (D-M5).
7. **Huecos** (definición en `docs/decisions.md`, I7): 6.434 huecos de más de
   3 min que suman 20.560 h en 2017–2022. El cálculo se ha verificado:
   el periodo total se reparte exactamente entre intervalos normales y
   huecos (`outputs/tables/quality_gap_accounting.csv`).
8. **Valores de la respuesta pendientes de interpretación**: registros por
   encima de la potencia nominal (máx. 3.713 W) y discrepancias aisladas
   entre `Power out` y `Power reg` (R1 y R2 en `docs/decisions.md`).

## 2. Anomalías documentadas y no interpretadas

| Observación | Dato | Por qué no se corrige |
|---|---|---|
| `Power out` > 1.800 W nominales (máx. 3.713 W) y, en su caso, > `Power max` | 2017–2022 | La documentación proporcionada no lo explica; ver R1 |
| `Log Time − Inv Time` ≈ +4,99 h, constante | 2017 | Si `Log Time` fuera hora local de São Paulo y `Inv Time` GMT, la diferencia sería −3 h (−2 h con horario de verano). No hay documentación que lo explique |
| Contador `watt-hours` desciende (683 veces en 2017, 1 Wh, con potencia nula; resumen 2017–2022 en `quality_counter_decreases.csv`) | | El diccionario lo define como acumulado; la causa no está documentada |
| `Event count` desciende 2 veces | 15/12 y 18/12/2017 | Ídem |
| Separador de ms alterna `.`/`,` en `Log Time` | desde el registro 17 716 | Corregido técnicamente (T1); no afecta al valor |
| `Line Frequency` = 0 | 2 registros en 2017 | No documentado; solo se señala |

Ninguna de estas anomalías se considera un fallo sin evidencia adicional.

## 3. Propuestas preliminares de preguntas de investigación

> **PROVISIONALES.** Basadas exclusivamente en lo observado en los datos.
> La pregunta final la deciden los tres miembros del grupo.

**P1 — Curva de potencia (regresión lineal múltiple con términos no
lineales).** ¿Cómo depende la potencia de salida, cuando la turbina produce,
de la velocidad de giro del rotor? ¿Qué forma funcional (polinómica,
transformación logarítmica) describe adecuadamente la relación y cuánta
variabilidad explica?
*Pros:* relación muy clara en el EDA; encaja con el Tema 6 (regresión
polinómica, transformaciones, diagnóstico).
*Contras:* la relación es casi determinista (mecánica), R² muy alto
esperable; poca "pregunta" estadística si no se añaden otros factores.

**P2 — Factores adicionales a la velocidad de giro.** Controlando por la
velocidad de giro, ¿se asocian la temperatura en la góndola, las condiciones
de la red (tensión, frecuencia), la hora del día o el año con diferencias en
la potencia producida?
*Pros:* pregunta inferencial con interés (eficiencia, degradación entre
años); permite interpretar coeficientes parciales.
*Contras:* efectos pequeños frente a RPM; con n muy grande todo será
"significativo" → hay que hablar de relevancia práctica y de la dependencia
temporal.

**P3 — Probabilidad de producción (GLM binomial).** ¿Qué variables explican
que en un minuto dado la turbina esté produciendo (P > 0)?
*Pros:* aprovecha el 75 % de ceros; modelo lineal generalizado (Tema 6,
regresión logística).
*Contras:* los códigos de estado y las RPM la explican casi perfectamente
(riesgo de separación); habría que excluir predictoras tautológicas.

**P4 — Evolución entre años del rendimiento.** Para una misma velocidad de
giro, ¿ha cambiado la potencia media producida entre 2018 y 2022?
*Pros:* usa todos los años y aporta una pregunta propia; no hemos
comprobado si el artículo de datos la aborda.
*Contras:* depende de disponer y validar los seis ficheros; confusión con
cambios de huecos o estados entre años.

Combinaciones posibles: P1 + P2 (un único lm con RPM y otras covariables) o
P3 + P1 (modelo en dos partes).

## 4. Cuestiones abiertas para Álvaro y Daniel

* Decidir primero la población de análisis (D-M6) y después la respuesta y
  los filtros (D-M1 a D-M5 en `docs/decisions.md`).
* Si se modeliza P > 0, explicarlo como decisión sobre la población
  (condicional a producción).
* No incluir a la vez RPM y `Windspeed (ref)`.
* Con n ~ 10⁶ registros, los contrastes de normalidad rechazarán siempre:
  priorizar gráficos de diagnóstico; discutir el efecto de la dependencia
  temporal en errores estándar y p-valores.
* Las observaciones influyentes deben contrastarse con los diagnósticos de
  calidad (huecos, códigos raros como `Turbine status` = 256) y con R1
  (registros por encima de la potencia nominal) antes de excluirlas.
