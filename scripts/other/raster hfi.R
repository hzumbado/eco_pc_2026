# Reparar raster HFI
# Profesor Hector Zumbado Ulate

# setup -------------------------------------------------------------------

rm(list = ls())

library(terra)

# hfi ---------------------------------------------------------------------

hfi <-
  rast('rasters/hfi_2020.tif')

hfi

# convert datatype --------------------------------------------------------

hfi <-
  hfi * 1.0

# check -------------------------------------------------------------------

minmax(hfi)

global(
  hfi,
  'isNA',
  na.rm = FALSE)

# save --------------------------------------------------------------------

writeRaster(
  hfi,
  'rasters/hfi_2020_clean.tif',
  datatype = 'FLT4S',
  NAflag = -9999,
  overwrite = TRUE)


rm(list = ls())

library(terra)

hfi <-
  rast('rasters/hfi_2020_clean.tif')

hfi

