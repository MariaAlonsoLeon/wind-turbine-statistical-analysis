# Producción de una pequeña turbina eólica urbana
## Análisis estadístico reproducible de datos SCADA con R

Proyecto de la **Actividad 1 de Entornos de Computación Estadística** del **Máster en Estadística Aplicada de la Universidad de Granada (curso 2026–2027)**.

**Autores:** Álvaro Rodríguez Gallardo · Daniel Vangente Viegas Alonso · María Alonso León

---

## Descripción

Este repositorio contiene un flujo reproducible en **R** para auditar, preparar, explorar y modelizar registros SCADA de una pequeña turbina eólica urbana **Skystream 3.7** instalada en la Universidad de São Paulo.

El análisis parte de registros minuto a minuto de **2017–2022** y estudia, durante los periodos de producción sostenida, la relación entre la potencia de salida y la velocidad de giro del rotor, así como la posible contribución adicional de la temperatura de la góndola y la estación del año.

La pregunta de investigación final es:

> **Entre los periodos en los que la turbina genera potencia, ¿cómo se relaciona la potencia de salida con la velocidad de giro del rotor y, para una misma velocidad de giro, existen diferencias asociadas a la temperatura de la góndola o a la estación del año?**

El informe se genera automáticamente con **R Markdown** a partir de los datos originales y de funciones reutilizables, sin pasos manuales intermedios.

---

## Enfoque estadístico

El flujo de análisis incluye:

- auditoría e integridad de los ficheros originales;
- limpieza técnica y validación del esquema;
- análisis descriptivo y exploratorio;
- estudio de huecos y dependencia temporal;
- definición reproducible de la población de análisis;
- agregación en intervalos de 10 minutos de producción sostenida;
- regresión múltiple sobre `log(Power out)`;
- modelización no lineal mediante un término cuadrático en `log(RPM)`;
- interpretación mediante elasticidades y efectos porcentuales;
- inferencia y análisis de sensibilidad frente a dependencia temporal;
- diagnóstico de residuos, multicolinealidad e influencia;
- comparación de modelos mediante \(R^2\) ajustado, AIC, BIC y contrastes F;
- selección de una especificación parsimoniosa.

El análisis completo distingue explícitamente entre **correcciones técnicas**, que no cambian la población estudiada, y **decisiones metodológicas**, documentadas en [`docs/decisions.md`](docs/decisions.md).

---

## Resultados principales

En la versión actual del análisis:

- la velocidad de giro del rotor explica la mayor parte de la variabilidad de `log(Power out)` durante los periodos de producción sostenida;
- la relación potencia–RPM es claramente no lineal y se representa adecuadamente mediante un término cuadrático en `log(RPM)`;
- la temperatura de la góndola y la estación del año aportan muy poca capacidad explicativa adicional;
- sus efectos no se mantienen como estadísticamente significativos en la submuestra temporalmente espaciada utilizada como análisis de sensibilidad;
- no se identifican observaciones con influencia global suficiente como para justificar su eliminación;
- dentro de los modelos candidatos evaluados, la especificación cuadrática en `log(RPM)` constituye la alternativa más parsimoniosa.

Estos resultados deben interpretarse teniendo en cuenta la presencia de huecos temporales, heterocedasticidad moderada y autocorrelación residual en la serie original.

---

## Datos

**Fuente:** Bassi, W., Rodrigues, A. L. y Sauer, I. L. (2023). *Operation SCADA Dataset of an Urban Small Wind Turbine in São Paulo, Brazil*. Zenodo.  
DOI: <https://doi.org/10.5281/zenodo.7348454> · Licencia: **CC BY 4.0**

**Artículo asociado:** Bassi, W., Rodrigues, A. L. y Sauer, I. L. (2023). *Dataset on SCADA Data of an Urban Small Wind Turbine Operation in São Paulo, Brazil*. *Data*, 8(3), 52.  
DOI: <https://doi.org/10.3390/data8030052>

Los seis CSV originales ocupan aproximadamente **260 MB** y no se versionan en GitHub.

Para reproducir el análisis:

1. descargar los seis ficheros anuales de Zenodo;
2. copiarlos **sin modificar** en `data/raw/`;
3. conservar los nombres originales:

```text
data_swt_iee_usp_2017.csv
data_swt_iee_usp_2018.csv
data_swt_iee_usp_2019.csv
data_swt_iee_usp_2020.csv
data_swt_iee_usp_2021.csv
data_swt_iee_usp_2022.csv
```

El proyecto audita los seis años. La modelización principal utiliza **2018–2022**; 2017 se excluye del modelo por su cobertura temporal parcial.

Más información en [`data/README.md`](data/README.md).

> **Nota para la entrega académica:** aunque los CSV originales no se versionan en GitHub, el enunciado de la actividad exige incluir los datos junto con los archivos fuente en el ZIP/RAR entregado en PRADO.

---

## Reproducibilidad

### Requisitos

- **R 4.3 o superior**;
- **RStudio** recomendado;
- una distribución de **LaTeX** para generar el PDF;
- aproximadamente **8 GB de RAM recomendados** para trabajar con los seis años con margen suficiente.

El proyecto utiliza [`renv`](https://rstudio.github.io/renv/) para fijar las versiones de los paquetes. El archivo `renv.lock` fue generado con **R 4.3.3**.

### Instalación

Clonar o descargar el repositorio y abrir:

```text
wind-turbine-analysis.Rproj
```

Después, desde la raíz del proyecto:

```r
# Restaurar las versiones fijadas de los paquetes
renv::restore()

# Comprobar entorno, configuración y disponibilidad de los datos
source("scripts/00_setup.R")
```

Si no se dispone de LaTeX, puede instalarse TinyTeX desde R:

```r
install.packages("tinytex")
tinytex::install_tinytex()
```

---

## Ejecución rápida

Una vez colocados los CSV en `data/raw/`, el flujo final puede ejecutarse en este orden:

```r
# 1. Construcción y validación de los datos
source("scripts/01_build_dataset.R")

# 2. Análisis exploratorio
source("scripts/02_run_eda.R")

# 3. Modelo acordado, inferencia, sensibilidad, diagnóstico y selección
source("scripts/04_fit_agreed_model.R")

# 4. Generación del informe PDF
source("scripts/99_render_report.R")
```

El PDF final se genera en:

```text
reports/output/actividad1.pdf
```

También puede renderizarse directamente:

```r
rmarkdown::render("reports/actividad1.Rmd")
```

El propio R Markdown reconstruye los resultados necesarios si no están disponibles, siempre que los CSV originales se encuentren en `data/raw/`.

> `scripts/03_fit_models.R` se conserva como pipeline de modelización general/preliminar. El informe final utiliza el modelo acordado implementado en `R/12_agreed_model.R` y ejecutado mediante `scripts/04_fit_agreed_model.R`.

---

## Estructura del repositorio

```text
wind-turbine-statistical-analysis/
├── README.md
├── wind-turbine-analysis.Rproj
├── .Rprofile
├── renv.lock
├── renv/
│
├── config/
│   └── config.yml                  # rutas y parámetros del análisis
│
├── data/
│   ├── README.md                   # instrucciones de descarga
│   ├── raw/                        # CSV originales; no versionados
│   │   └── data_description.txt
│   ├── processed/                  # generado: limpieza técnica
│   └── final/                      # generado: conjunto de análisis
│
├── R/
│   ├── 00_utils.R                  # utilidades generales
│   ├── 01_io.R                     # lectura y esquema
│   ├── 02_cleaning.R               # limpieza y filtros
│   ├── 03_validation.R             # controles de calidad
│   ├── 04_features.R               # variables derivadas
│   ├── 05_eda.R                    # cálculos exploratorios
│   ├── 06_modeling.R               # infraestructura de modelización
│   ├── 07_inference.R              # inferencia
│   ├── 08_diagnostics.R            # diagnóstico
│   ├── 09_model_selection.R        # selección de modelos
│   ├── 10_tables.R                 # tablas del informe
│   ├── 11_plots.R                  # tema y gráficos del informe
│   └── 12_agreed_model.R           # modelo final acordado por el grupo
│
├── scripts/
│   ├── 00_setup.R                  # comprobación del entorno
│   ├── 01_build_dataset.R          # raw -> processed/final
│   ├── 02_run_eda.R                # EDA y salidas descriptivas
│   ├── 03_fit_models.R             # modelización general/preliminar
│   ├── 04_fit_agreed_model.R       # análisis estadístico final
│   └── 99_render_report.R          # renderizado del PDF
│
├── reports/
│   ├── actividad1.Rmd              # informe reproducible
│   ├── preamble.tex                # estilo LaTeX
│   ├── references.bib              # bibliografía
│   ├── logo_ugr.png
│   └── output/                     # generado; no versionado
│
├── outputs/                         # figuras, tablas y modelos generados
│
├── docs/
│   ├── data_dictionary.md          # diccionario de variables
│   ├── decisions.md                # decisiones técnicas y metodológicas
│   └── methodology_notes.md        # notas metodológicas
│
└── tests/
    ├── testthat.R
    └── testthat/                   # tests unitarios e integración
```

### Principio de diseño

```text
R/       = lógica reutilizable
scripts/ = pipelines reproducibles
Rmd      = narrativa + llamadas a funciones + resultados
```

El informe no contiene grandes bloques de procesamiento o modelización: las decisiones y cálculos viven en funciones reutilizables y quedan documentados fuera del texto narrativo.

---

## Validación y tests

La batería de tests incluye comprobaciones de:

- lectura y compatibilidad de los CSV;
- integridad de los datos originales;
- limpieza y transformación;
- validación del esquema;
- construcción de variables;
- cálculos del EDA;
- funciones de modelización e inferencia;
- pipeline del modelo acordado;
- estilo de tablas y gráficos.

Para ejecutar todos los tests:

```r
source("tests/testthat.R")
```

Los tests utilizan pequeños *fixtures* incluidos en `tests/testthat/fixtures/`, por lo que varias comprobaciones pueden ejecutarse sin versionar los CSV completos.

---

## Convenciones de presentación

Para mantener una identidad visual única en todo el informe:

- **gráficos:** `theme_wind_report()` y `wind_palette()` en `R/11_plots.R`;
- **tablas:** `style_report_table()` en `R/10_tables.R`;
- **formato numérico:** `fmt_num()`, `fmt_int()` y `fmt_pct()`;
- los títulos y pies se gestionan desde R Markdown/knitr;
- los nombres originales de las columnas se conservan en código y se explican en español en el texto.

---

## Trazabilidad de las decisiones

Las decisiones del proyecto se documentan en [`docs/decisions.md`](docs/decisions.md), distinguiendo entre:

- **T\***: correcciones técnicas;
- **I\***: decisiones de implementación;
- **D-M\***: decisiones metodológicas del análisis;
- **D-D\***: decisiones posteriores de diagnóstico y simplificación.

Esta separación permite reconstruir qué transformaciones son puramente técnicas y cuáles modifican la población, la especificación o la interpretación estadística.

---

## Generación del informe

El documento académico se encuentra en:

```text
reports/actividad1.Rmd
```

El PDF se genera con R Markdown, `knitr`, Pandoc y LaTeX. El preámbulo personalizado (`reports/preamble.tex`) controla el formato tipográfico, tablas, cabeceras y otros detalles editoriales.

El informe final respeta el límite de **15 páginas, excluyendo portada y referencias**, establecido en el enunciado de la actividad.

---

---

## Referencias principales

- Bassi, W., Rodrigues, A. L. & Sauer, I. L. (2023). *Operation SCADA Dataset of an Urban Small Wind Turbine in São Paulo, Brazil*. Zenodo. <https://doi.org/10.5281/zenodo.7348454>
- Bassi, W., Rodrigues, A. L. & Sauer, I. L. (2023). *Dataset on SCADA Data of an Urban Small Wind Turbine Operation in São Paulo, Brazil*. *Data*, 8(3), 52. <https://doi.org/10.3390/data8030052>
- Faraway, J. J. (2004). *Linear Models with R*. Chapman & Hall/CRC.
- Crawley, M. J. (2015). *Statistics: An Introduction Using R*. Wiley.

La bibliografía completa del informe se encuentra en [`reports/references.bib`](reports/references.bib).

