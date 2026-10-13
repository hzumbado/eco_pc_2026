# Ecologia de poblaciones y comunidades
# Profesor Hector Zumbado Ulate
# Simulacion de datos distribucion espacial

# setup ----------------------------------------------------------------

rm(list = ls())

library(sf)
library(tmap)
library(tidyverse)

# Área de estudio ---------------------------------------------------------

area <-
  st_bbox(
    c(
      xmin = 0,
      ymin = 0,
      xmax = 100,
      ymax = 100),
    crs = NA) %>%
  st_as_sfc()

area %>%
  tm_shape() +
  tm_polygons()


# Cuadrículas de muestreo -------------------------------------------------

set.seed(2026)

cuadriculas <-
  crossing(
    fila = 1:5,
    columna = 1:10) %>%
  mutate(
    id = row_number(),
    x_min = (columna - 1) * 10,
    y_min = (fila - 1) * 20,
    x = runif(
      n(),
      x_min + 0.5,
      x_min + 9.5),
    y = runif(
      n(),
      y_min + 0.5,
      y_min + 19.5)) %>%
  st_as_sf(
    coords = c('x', 'y'),
    remove = FALSE)

# Mapa de cuadrículas -----------------------------------------------------

tm_shape(area) +
  tm_polygons(
    fill = 'grey95',
    col = 'grey50') +
  tm_shape(cuadriculas) +
  tm_symbols(
    size = 0.4,
    fill = 'black') +
  tm_text(
    'id',
    size = 0.6,
    ymod = 0.7) +
  tm_layout(
    frame = FALSE)


# Elephantopus mollis -----------------------------------------------------

# Centros de agregación

parches <-
  tibble(
    x = c(20, 55, 80),
    y = c(25, 70, 35))

# Distancia a los centros de agregación

datos_raw <-
  cuadriculas %>%
  st_drop_geometry() %>%
  mutate(
    d1 = sqrt(
      (x - parches$x[1])^2 +
        (y - parches$y[1])^2),
    d2 = sqrt(
      (x - parches$x[2])^2 +
        (y - parches$y[2])^2),
    d3 = sqrt(
      (x - parches$x[3])^2 +
        (y - parches$y[3])^2))

# Abundancia esperada

datos_parches <-
  datos_raw %>%
  mutate(
    lambda_elephantopus =
      1 +
      14 * exp(-(d1^2) / (2 * 15^2)) +
      16 * exp(-(d2^2) / (2 * 17^2)) +
      13 * exp(-(d3^2) / (2 * 14^2)))

# Abundancia observada

set.seed(2026)

datos_elephantopus <-
  datos_parches %>%
  mutate(
    elephantopus = rpois(
      n(),
      lambda_elephantopus))

# Resumen de abundancia

datos_elephantopus %>%
  summarise(
    minimo = min(elephantopus),
    media = mean(elephantopus),
    maximo = max(elephantopus),
    varianza = var(elephantopus),
    indice_dispersion = varianza / media)

# Cuadrículas con mayor abundancia

datos_elephantopus %>%
  select(
    id,
    x,
    y,
    elephantopus) %>%
  arrange(desc(elephantopus))


# Datos espaciales

elephantopus_sf <-
  datos_elephantopus %>%
  st_as_sf(
    coords = c('x', 'y'),
    remove = FALSE)

# Mapa de abundancia

tm_shape(area) +
  tm_polygons(
    fill = 'grey95',
    col = 'grey50') +
  tm_shape(elephantopus_sf) +
  tm_symbols(
    size = 'elephantopus',
    fill = 'black',
    col = 'black',
    size.scale = tm_scale_continuous(
      values.scale = 1.5),
    size.legend = tm_legend(
      title = 'Individuos/m²')) +
  tm_layout(
    frame = FALSE)

# Rhynchospora nervosa ----------------------------------------------------

set.seed(2027)

datos_rhynchospora <-
  datos_elephantopus %>%
  mutate(
    rhynchospora = rpois(
      n(),
      lambda = 2))

# Resumen de abundancia

datos_rhynchospora %>%
  summarise(
    minimo = min(rhynchospora),
    media = mean(rhynchospora),
    maximo = max(rhynchospora),
    varianza = var(rhynchospora),
    indice_dispersion = varianza / media)

# Datos espaciales

rhynchospora_sf <-
  datos_rhynchospora %>%
  st_as_sf(
    coords = c('x', 'y'),
    remove = FALSE)


# Mapa de abundancia

tm_shape(area) +
  tm_polygons(
    fill = 'grey95',
    col = 'grey50') +
  tm_shape(rhynchospora_sf) +
  tm_symbols(
    size = 'rhynchospora',
    fill = 'black',
    col = 'black',
    size.scale = tm_scale_continuous(
      values.scale = 1.5),
    size.legend = tm_legend(
      title = 'Individuos/m²')) +
  tm_layout(
    frame = FALSE)

# Datos finales -----------------------------------------------------------

datos_final <-
  datos_rhynchospora %>%
  select(
    id,
    x,
    y,
    elephantopus,
    rhynchospora)

datos_final

# Guardar datos -----------------------------------------------------------

write_rds(
  datos_final,
  'data/processed/datos_distribucion_espacial.rds')
