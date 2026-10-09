# Producción de una pequeña turbina eólica urbana: análisis estadístico reproducible en R

Actividad 1 de **Entornos de Computación Estadística** — Máster en
Estadística Aplicada, Universidad de Granada (curso 2026-27).

**Autores:** María Alonso León · Álvaro [Apellidos] · Daniel [Apellidos]

## Objetivo

Analizar y modelizar estadísticamente la potencia producida por una turbina
Skystream 3.7 (1,8 kW) instalada en la Universidad de São Paulo, a partir de
sus registros SCADA minuto a minuto (2017–2022): preparación documentada de
los datos, análisis exploratorio y un modelo de regresión lineal (o lineal
generalizado) con inferencia, diagnóstico y selección de variables. El
resultado es un informe PDF dinámico (máx. 15 páginas) generado con
R Markdown.

## Datos

* Fuente: Bassi, W., Rodrigues, A. L. y Sauer, I. L. (2023). *Operation SCADA
  Dataset of an Urban Small Wind Turbine in São Paulo, Brazil*. Zenodo.
  <https://doi.org/10.5281/zenodo.7348454> (CC BY 4.0).
* Artículo: *Data* 8(3):52, <https://doi.org/10.3390/data8030052>.

> ⚠️ **Los CSV originales (~260 MB) no se suben a GitHub.** Descárgalos y
> colócalos en `data/raw/` siguiendo [`data/README.md`](data/README.md). Nunca
> los edites: todo el procesamiento trabaja sobre copias en memoria.

## Estructura

```
wind-turbine-statistical-analysis/
├── README.md
├── wind-turbine-analysis.Rproj      Abrir este fichero en RStudio
├── .Rprofile, renv.lock, renv/      Versiones fijadas de los paquetes (renv)
├── config/config.yml                Rutas y parámetros; filtros metodológicos
├── data/
│   ├── README.md                    Descarga y estructura de los datos
│   ├── raw/                         Originales (solo lectura, no versionados)
│   ├── processed/                   (generado) correcciones técnicas
│   └── final/                       (generado) conjunto del modelo
├── R/                               LÓGICA: funciones reutilizables
│   ├── 00_utils.R                   Paquetes, configuración, formato, guardado
│   ├── 01_io.R                      Esquema de columnas, lectura, validación de esquema
│   ├── 02_cleaning.R                Correcciones técnicas / filtros metodológicos / pipeline
│   ├── 03_validation.R              Calidad de datos (identifica, no elimina)
│   ├── 04_features.R                Variables derivadas
│   ├── 05_eda.R                     Cálculos exploratorios
│   ├── 06_modeling.R                [Álvaro] ajuste            (interfaz pendiente)
│   ├── 07_inference.R               [Álvaro] inferencia        (interfaz pendiente)
│   ├── 08_diagnostics.R             [Daniel] diagnóstico       (interfaz pendiente)
│   ├── 09_model_selection.R         [Daniel] selección         (interfaz pendiente)
│   ├── 10_tables.R                  Tablas formateadas para el informe
│   └── 11_plots.R                   Gráficos para el informe
├── scripts/                         PIPELINES (se ejecutan en orden)
│   ├── 00_setup.R                   Comprueba entorno y datos
│   ├── 01_build_dataset.R           raw -> processed / final + informe de calidad
│   ├── 02_run_eda.R                 Todas las tablas y figuras exploratorias
│   ├── 03_fit_models.R              [Álvaro/Daniel] modelos (esqueleto)
│   └── 99_render_report.R           Genera el PDF
├── reports/
│   ├── actividad1.Rmd               INFORME: narrativa + llamadas a funciones
│   ├── preamble.tex                 Estilo LaTeX
│   ├── references.bib               Bibliografía
│   └── output/                      (generado) actividad1.pdf
├── outputs/                         (generado) tables/, figures/, models/
├── docs/
│   ├── data_dictionary.md           Diccionario de variables y códigos
│   ├── decisions.md                 Registro de decisiones (T*, I*, D-M*)
│   └── methodology_notes.md         Hallazgos, anomalías y propuestas de pregunta
└── tests/testthat/                  Tests de las funciones de datos
```

Principio de diseño: **`R/` = lógica · `scripts/` = pipelines · `Rmd` =
narrativa**. El informe solo llama a funciones; ninguna decisión metodológica
está oculta: los filtros se activan en `config/config.yml` y se justifican en
`docs/decisions.md`.

### Cambios respecto a la estructura inicial propuesta

* **Sin `data/interim/`**: la combinación de años tarda segundos; guardarla
  duplicaría ~1 GB sin ganar trazabilidad.
* **Sin `outputs/logs/`**: el registro de limpieza se guarda como tabla
  (`outputs/tables/cleaning_log.csv`).
* **Carpetas generadas no versionadas** (`data/processed`, `data/final`,
  `outputs/*`, `reports/output`): las crean los scripts, no existen vacías.
* **`reports/preamble.tex`** añadido para el estilo del PDF.

## Instalación

Requisitos: R ≥ 4.3, RStudio (recomendado), LaTeX para el PDF
(`install.packages("tinytex"); tinytex::install_tinytex()`), y ~8 GB de RAM
para los seis años (pico medido: ~2,1 GB con 1,7 M de filas).

```r
# 1. Abrir wind-turbine-analysis.Rproj (renv se activa solo vía .Rprofile)
# 2. Instalar exactamente las versiones de renv.lock
renv::restore()
# 3. Comprobar entorno y datos
source("scripts/00_setup.R")
```

`renv.lock` se generó con R 4.3.3. Si se añade un paquete (p. ej. `car` o
`lmtest` para el diagnóstico), añadirlo a `project_packages()` en
`R/00_utils.R` y ejecutar `renv::snapshot()`.

## Uso

Desde la raíz del proyecto, en una sesión limpia de R:

```r
source("scripts/01_build_dataset.R")   # datos procesados + informe de calidad (~1-2 min)
source("scripts/02_run_eda.R")         # tablas y figuras exploratorias en outputs/ (~3 min)
source("scripts/03_fit_models.R")      # modelos (pendiente: Álvaro / Daniel)
source("scripts/99_render_report.R")   # reports/output/actividad1.pdf
```

El informe también puede generarse directamente con
`rmarkdown::render("reports/actividad1.Rmd")` o el botón *Knit*: si los datos
procesados no existen, el propio informe los construye.

Tests:

```r
source("tests/testthat.R")
```

## Convenciones de figuras y tablas (para los tres)

* **Figuras:** `theme_wind_report()` y `wind_palette()` (`R/11_plots.R`).
  Tres colores funcionales: azul `primary` (dato principal), ámbar
  `secondary` (referencias, siempre en trazo grueso o discontinuo) y gris
  `neutral`. Sin título ni pie dentro del gráfico: los pone `fig.cap`. Insertar
  a tamaño real (`fig.width = 6.38` a ancho completo; `3.16` y
  `out.width = "49.5%"` para dos paneles).
* **Tablas:** `style_report_table(df, caption, note)` (`R/10_tables.R`).
  Formatear antes los números con `fmt_num()`/`fmt_int()`; caption breve y
  definiciones en `note`; unidades en la cabecera.
* **Texto:** primera mención "potencia de salida (`Power out`)", después
  "potencia de salida"; nombres exactos en `código` solo al hablar de columnas.

## Trabajo en grupo

Ramas recomendadas:

```
main                          versión estable (compila y pasa los tests)
feature/maria-data            datos, calidad, EDA, secciones 2-3
feature/alvaro-model          R/06, R/07, sección 4 y 5.1-5.3
feature/daniel-diagnostics    R/08, R/09, secciones 5.4-5.6 y 6
```

Normas: no editar `data/raw/`; toda decisión nueva en `docs/decisions.md`;
fusionar en `main` solo si `source("tests/testthat.R")` y el
render del informe funcionan; las cajas **TODO** del PDF deben desaparecer
antes de la entrega.

## Estado

| Parte | Responsable | Estado |
|---|---|---|
| Datos, calidad, EDA, secciones 1 (contexto), 2 y 3 | María | implementado (verificado con 2017) |
| Pregunta de investigación y filtros (D-M1…D-M8) | Grupo | pendiente |
| Modelo, ajuste, inferencia (secciones 4, 5.1–5.3) | Álvaro | interfaz preparada |
| Diagnóstico, influyentes, selección, conclusiones | Daniel | interfaz preparada |
