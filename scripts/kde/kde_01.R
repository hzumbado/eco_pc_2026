# Ecologia de poblaciones y comunidades
# Profesor Hector Zumbado Ulate
# Densidad kernel de ocurrencias (KDE)

# setup -------------------------------------------------------------------

rm(list = ls())

library(terra)
library(sf)
library(tmap)
library(tidyverse)

# Instalar los paquetes de spatstat que no se puedan cargar.
# Esta seccion requiere Internet si faltan paquetes.

# paquetes <-
#   c('spatstat.utils', 'spatstat.data', 'spatstat.univar',
#     'spatstat.geom', 'spatstat.random', 'spatstat.sparse',
#     'spatstat.explore')
#
# faltantes <-
#   paquetes %>%
#   purrr::keep(~ !requireNamespace(.x, quietly = TRUE))
#
# if (length(faltantes) > 0) install.packages(faltantes, repos = 'https://cloud.r-project.org')

library(spatstat.geom)
library(spatstat.explore)


# shapefiles --------------------------------------------------------------

costa_rica <-
  read_sf('shapefiles/costa_rica_wgs84.gpkg') %>%
  st_make_valid() %>%
  st_transform(4326)

print(costa_rica)


# species -----------------------------------------------------------------

# Cambiar estos nombres para trabajar con otra especie.
# my_species debe coincidir con el nombre del archivo.

species <- 'Craugastor fitzingeri'
my_species <- 'craugastor_fitzingeri'


# data --------------------------------------------------------------------

occs <-
  read_rds(
    paste0(
      'data/processed/',
      my_species,
      '_occ_spatial.rds'))

occs

# inspect -----------------------------------------------------------------

nrow(occs)

occs %>%
  count(source)

# coordinates -------------------------------------------------------------

# x = longitud; y = latitud.
# Conservar el dataset original en occs.

occs_numeric <-
  occs %>%
  mutate(
    across(c(x, y),
           as.numeric)) %>%
  filter(
    is.finite(x),
    is.finite(y))

# duplicated coordinates --------------------------------------------------

occs_numeric %>%
  count(x, y) %>%
  filter(n > 1)

# Conservar una observacion por localidad.
# Las coordenadas repetidas no aportaran mas peso al KDE.

occs_unique <-
  occs_numeric %>%
  distinct(
    x,
    y,
    .keep_all = TRUE)

nrow(occs_unique)

# Eliminar duplicados no equivale a hacer rarefaccion espacial.
# No aplicamos rarefaccion sin una distancia justificada.
# El sesgo de muestreo todavia puede influir en el resultado.


# spatial object ----------------------------------------------------------

occs_sf <-
  occs_unique %>%
  st_as_sf(
    coords =
      c('x', 'y'),
    crs = 4326,
    remove = FALSE)

# Conservar las localidades que intersectan Costa Rica.

occs_cr <-
  occs_sf %>%
  filter(
    lengths(
      st_intersects(
        .,
        costa_rica)) > 0)

nrow(occs_cr)

if (nrow(occs_cr) < 3) stop('Se necesitan al menos tres localidades para esta practica.')

# occurrence map ----------------------------------------------------------

occurrence_map <-
  tm_shape(costa_rica) +
  tm_polygons(fill = '#a0a479') +
  tm_shape(occs_cr) +
  tm_dots(
    fill = 'black',
    size = 0.25)

occurrence_map

# projected coordinates ---------------------------------------------------

# El KDE necesita distancias en metros.
# EPSG 5367 corresponde a CR05 / CRTM05, una proyeccion para Costa Rica.

costa_rica_m <-
  costa_rica %>%
  st_transform(5367)

occs_m <-
  occs_cr %>%
  st_transform(5367)

occ_coordinates <-
  st_coordinates(occs_m)

# KDE parameters ----------------------------------------------------------

# sigma controla el suavizado del kernel gaussiano.
# Valores menores conservan detalles locales.
# Valores mayores producen superficies mas suavizadas.
# Los 5000 m son un valor inicial para esta practica.

sigma <- 1000

# Tamano aproximado de las celdas, en metros.
# Resolucion y ancho de banda son parametros diferentes.

resolution <- 500

# observation window ------------------------------------------------------

# Crear una ventana rectangular con un margen alrededor de Costa Rica.
# Es un limite de calculo, no un area de muestreo conocida.

country_limits <-
  st_bbox(costa_rica_m)

kde_window <-
  owin(
    xrange = c(country_limits[['xmin']], country_limits[['xmax']]) + c(-4, 4) * sigma,
    yrange = c(country_limits[['ymin']], country_limits[['ymax']]) + c(-4, 4) * sigma)

# point pattern -----------------------------------------------------------

# Crear el objeto de puntos requerido por spatstat.

occ_points <-
  ppp(
    x = occ_coordinates[, 1],
    y = occ_coordinates[, 2],
    window = kde_window)

# KDE ---------------------------------------------------------------------

# Este resultado es un objeto im de spatstat, todavia no un raster.
# No aplicar correccion de borde a una ventana de calculo arbitraria.

kde_image <-
  density(
    occ_points,
    sigma = sigma,
    eps = resolution,
    edge = FALSE)

kde_image

# KDE table ---------------------------------------------------------------

# Convertir la superficie en una tabla con coordenadas y densidad.

kde_table <-
  kde_image %>%
  as.data.frame() %>%
  select(x, y, value)

print(kde_table)

# KDE raster --------------------------------------------------------------

# Crear el raster completo a partir de la tabla.

kde_raster <-
  kde_table %>%
  rast(
    type = 'xyz',
    crs =
      st_crs(costa_rica_m)$wkt)

# Recortar a la extension de Costa Rica.

kde_crop <-
  kde_raster %>%
  crop(vect(costa_rica_m))

# Excluir las celdas fuera del territorio nacional.

kde_mask <-
  kde_crop %>%
  mask(vect(costa_rica_m))

# Corregir pequenos valores negativos por aproximacion numerica.
# Este es el raster final en localidades por m².

kde_density <-
  kde_mask %>%
  clamp(lower = 0)

names(kde_density) <- 'density'

# Crear una version en localidades por km² para el mapa.

kde_km2 <-
  kde_density * 1000000

names(kde_km2) <- 'localities_km2'

# KDE map -----------------------------------------------------------------

kde_map <-
  tm_shape(kde_km2) +
  tm_raster(
    col.scale =
      tm_scale_continuous(
        values = c(
          '#fff4c2',
          '#c79b28',
          '#347a85',
          '#16384f')),
    col.legend =
      tm_legend(
        title = 'Localidades / km²')) +
  tm_shape(costa_rica_m) +
  tm_borders(col = 'gray40') +
  tm_shape(occs_m) +
  tm_dots(
    fill = 'black',
    size = 0.08) +
  tm_scalebar(
    position = c('left', 'bottom')) +
  tm_layout(
    frame = FALSE,
    legend.outside = TRUE)

kde_map

# Valores altos indican concentracion de localidades registradas.
# No representan abundancia de organismos ni idoneidad del habitat.

# cumulative density ------------------------------------------------------

# Ordenar las celdas desde mayor hasta menor densidad.
# Las celdas tienen igual area, por lo que podemos sumar sus valores.
# Calcular la proporcion acumulada de la masa del KDE.
# Las proporciones corresponden a la superficie retenida en Costa Rica.

kde_cumulative <-
  tibble(
    density =
      as.vector(
        values(kde_density))) %>%
  filter(
    is.finite(density), density > 0) %>%
  arrange(desc(density)) %>%
  mutate(
    cumulative =
      cumsum(density) / sum(density))

kde_levels <- c(0.50, 0.95)

# Identificar la densidad donde se alcanza cada proporcion acumulada.

kde_thresholds <-
  kde_levels %>%
  map_dbl(~ kde_cumulative$density[which(kde_cumulative$cumulative >= .x)[1]])

kde_summary <-
  tibble(
    level = kde_levels,
    threshold = kde_thresholds)

kde_summary

# KDE polygons ------------------------------------------------------------

# .x = nivel (50% o 95%).
# .y = umbral de densidad correspondiente.

kde_polygon_list <-
  map2(
    kde_levels,
    kde_thresholds,
    ~ ifel(kde_density >= .y, 1, NA) %>%
      as.polygons(dissolve = TRUE, na.rm = TRUE) %>%
      st_as_sf() %>%
      st_make_valid() %>%
      select(geometry) %>%
      mutate(level = .x, threshold = .y))

# Unir los resultados del 50% y 95% en un solo objeto.

kde_polygons <-
  kde_polygon_list %>%
  bind_rows()

# El 50% delimita las zonas de mayor concentracion.
# El 95% incluye una region mas amplia.
# Son porcentajes de masa del KDE, no necesariamente de registros.
# Cada region puede contener varios sectores separados.


# polygon map -------------------------------------------------------------

polygon_map <-
  tm_shape(costa_rica_m) +
  tm_polygons(fill = '#f3f1e8') +
  tm_shape(
    kde_polygons %>%
      filter(level == 0.95)) +
  tm_polygons(
    fill = '#c79b28',
    fill_alpha = 0.40,
    col = '#c79b28') +
  tm_shape(
    kde_polygons %>%
      filter(level == 0.50)) +
  tm_polygons(
    fill = '#16384f',
    fill_alpha = 0.65,
    col = '#16384f') +
  tm_shape(occs_m) +
  tm_dots(
    fill = 'black',
    size = 0.08) +
  tm_scalebar(
    position = c('left', 'bottom')) +
  tm_layout(frame = FALSE)

polygon_map

# Amarillo: region del 95%.
# Azul oscuro: region del 50%.

# output folders ----------------------------------------------------------

c('output/figures', 'output/spatial') %>%
  walk(~ dir.create(.x, recursive = TRUE, showWarnings = FALSE))

# save figures ------------------------------------------------------------

tmap_save(
  kde_map,
  filename =
    paste0(
      'output/figures/',
      my_species,
      '_kde_sigma_',
      sigma,
      '.jpg'),
  width = 12,
  height = 6.75,
  units = 'in',
  dpi = 300)

tmap_save(
  polygon_map,
  filename =
    paste0(
      'output/figures/',
      my_species,
      '_kde_polygons_sigma_',
      sigma,
      '.jpg'),
  width = 12,
  height = 6.75,
  units = 'in',
  dpi = 300)


# save spatial objects ----------------------------------------------------

writeRaster(
  kde_km2,
  filename =
    paste0(
      'output/spatial/',
      my_species,
      '_kde_sigma_',
      sigma,
      '.tif'),
  overwrite = TRUE)

st_write(
  kde_polygons,
  dsn =
    paste0(
      'output/spatial/',
      my_species,
      '_kde_polygons_sigma_',
      sigma,
      '.gpkg'),
  delete_dsn = TRUE)

write_rds(
  kde_polygons,
  paste0(
    'output/spatial/',
    my_species,
    '_kde_polygons_sigma_',
    sigma,
    '.rds'))
# interpretation ----------------------------------------------------------

# Repetir desde KDE parameters con sigma = 1000, 2000 y 10000.
# Comparar las superficies y los poligonos.
#
# ¿Que concentraciones permanecen al cambiar el ancho de banda?
# ¿Cuales aparecen solamente con un suavizado menor?
# ¿Coinciden con sitios de mayor esfuerzo de muestreo?
#
# Una resolucion mas fina no elimina el sesgo de muestreo.
# Una zona sin registros no demuestra ausencia de la especie.
# Los poligonos no son limites comprobados de distribucion.
