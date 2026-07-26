# Ecologia de poblaciones y comunidades 2026
# Estructura básica del proyecto
# Profesor Héctor Zumbado Ulate

# setup -------------------------------------------------------------------

rm(list = ls())

folders <-
  c('data/raw',
    'data/processed',
    'scripts',
    'scripts/other',
    'shapefiles',
    'rasters',
    'output/figures',
    'output/tables',
    'docs',
    'other')

sapply(
  folders,
  FUN = dir.create,
  recursive = TRUE)
