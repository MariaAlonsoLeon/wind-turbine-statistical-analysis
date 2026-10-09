# Datos

## Origen

**Operation SCADA Dataset of an Urban Small Wind Turbine in São Paulo, Brazil**
W. Bassi, A. L. Rodrigues, I. L. Sauer (2023). Zenodo.
DOI: [10.5281/zenodo.7348454](https://doi.org/10.5281/zenodo.7348454) ·
Licencia: CC BY 4.0 ·
Artículo descriptivo: *Data* 8(3):52, DOI [10.3390/data8030052](https://doi.org/10.3390/data8030052).

## Ficheros necesarios

Descargar de la página de Zenodo y copiar **sin modificar** en `data/raw/`:

| Fichero | Tamaño aprox. | ¿Versionado en git? |
|---|---|---|
| `data_swt_iee_usp_2017.csv` | 5,3 MB | No |
| `data_swt_iee_usp_2018.csv` | 45,1 MB | No |
| `data_swt_iee_usp_2019.csv` | 54,7 MB | No |
| `data_swt_iee_usp_2020.csv` | 40,5 MB | No |
| `data_swt_iee_usp_2021.csv` | 46,7 MB | No |
| `data_swt_iee_usp_2022.csv` | 66,3 MB | No |
| `data_description.txt` | 3 KB | **Sí** |

El proyecto funciona con cualquier subconjunto de años (`scripts/00_setup.R`
avisa de los que faltan), pero el análisis final debe usar los seis.

## Estructura

```
data/
├── raw/          Originales, SOLO LECTURA. Nunca se modifican.
│                 Recomendado: marcarlos como de solo lectura en el sistema.
├── processed/    swt_scada_clean.rds  -> solo correcciones técnicas (T1-T5)
└── final/        swt_analysis_dataset.rds -> + filtros metodológicos (D-M*)
```

`processed/` y `final/` se generan con `source("scripts/01_build_dataset.R")`
y no se versionan. No existe capa `interim/`: la lectura y combinación de los
años tarda segundos, y guardarla duplicaría datos sin aportar trazabilidad.
Hasta que el grupo decida los filtros (`docs/decisions.md`), `final` es igual
que `processed`.

## Integridad

`build_datasets()` calcula la huella MD5 de cada fichero original antes y
después del proceso y se detiene si cambia. Las huellas quedan en
`outputs/tables/quality_raw_manifest.csv`. Huella del fichero de 2017
utilizado: `8fbc1a6e3436c1284d88009810f3fbc8`.

## Entrega en PRADO

El enunciado exige entregar el zip con **todos** los ficheros fuente,
incluidos los datos: para la entrega final, copiar los CSV en `data/raw/`
dentro del zip aunque no estén en git.
