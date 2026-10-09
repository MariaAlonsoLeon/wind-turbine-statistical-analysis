# =============================================================================
# 01_build_dataset.R — Construcción reproducible de los conjuntos de datos
# -----------------------------------------------------------------------------
# Responsable: María
# Uso:  source("scripts/01_build_dataset.R")   (o Rscript scripts/01_build_dataset.R)
#
# Entradas : data/raw/data_swt_iee_usp_YYYY.csv  (solo lectura)
# Salidas  : data/processed/swt_scada_clean.rds    (correcciones técnicas)
#            data/final/swt_analysis_dataset.rds   (filtros de config.yml)
#            outputs/tables/quality_*.csv, cleaning_log.csv
# =============================================================================

source(here::here("R", "00_utils.R"))
load_project_packages()
source_project_functions()

result <- build_datasets(load_config())

print(quality_overview(result$checks), n = Inf, width = Inf)
