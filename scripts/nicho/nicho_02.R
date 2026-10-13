# Ecologia de poblaciones y comunidades
# Profesor Hector Zumbado Ulate
# Nicho bidimensional 2 (ejemplo decline)

# setup -------------------------------------------------------------------

rm(list = ls())

library(terra)
library(sf)
library(tmap)
library(tidyverse)

# shapefiles --------------------------------------------------------------

list.files(
  'shapefiles',
  pattern = '(cos|san)',
  full.names = TRUE) %>%
  map(
    ~.x %>%
      read_sf() %>%
      st_make_valid()) %>%
  set_names(
    'costa_rica',
    'santa_elena') %>%
  list2env(.GlobalEnv)

# data --------------------------------------------------------------------

my_species <-
  'craugastor_ranoides'

occs <-
  read_rds(
    paste0(
    'data/processed/',
    my_species,
    '_occ_clean.rds')) %>%
  mutate(
    status = if_else(
      year < 2000,
      'before_2000',
      'since_2000'))

occs_sf <-
  occs %>%
  st_as_sf(
    coords = c('x', 'y'),
    crs = 4326,
    remove = FALSE)

# map ---------------------------------------------------------------------

costa_rica %>%
  tm_shape() +
  tm_grid(lines = FALSE) +
  tm_polygons(col = '#a0a479') +
  tm_shape(santa_elena) +
  tm_grid(lines = FALSE) +
  tm_borders(col = 'darkgreen') +
  tm_shape(
    occs_sf %>%
      filter(status == 'before_2000')) +
  tm_dots(
    col = 'red',
    shape = 21,
    size = 0.3) +
  tm_shape(
    occs_sf %>%
      filter(status == 'since_2000')) +
  tm_dots(
    col = 'blue',
    shape = 21,
    size = 0.3)

# environmental rasters ---------------------------------------------------

list.files(
  'rasters',
  pattern = '(wc_|elev_|hf)',
  full.names = TRUE) %>%
  map(rast) %>%
  set_names(
    'elevation',
    'hfi',
    'wc') %>%
  list2env(.GlobalEnv)

# raster geometry ---------------------------------------------------------

print(elevation)
plot(elevation)

print(hfi)
plot(hfi)

print(wc)
plot(wc)
plot(wc$BIO_01) # temperatura anual promedio
plot(wc$BIO_12) # precipitacion anual promedio

crs(wc$BIO_01, proj = TRUE)
crs(elevation, proj = TRUE)
crs(hfi, proj = TRUE)

res(wc$BIO_01)
res(elevation)
res(hfi)

compareGeom(
  wc$BIO_01,
  elevation)

compareGeom(
  wc$BIO_01,
  hfi)

# same resolution and grid, different extent ------------------------------

elevation <-
  crop(
    elevation,
    wc$BIO_01)

compareGeom(
  wc$BIO_01,
  elevation)

# resample rasters --------------------------------------------------------

hfi <-
  resample(
    hfi,
    wc$BIO_01,
    method = 'bilinear')

compareGeom(
  wc$BIO_01,
  hfi)

# environmental variables ------------------------------------------------

environmental_rasters <-
  c(
    wc$BIO_01,
    wc$BIO_12,
    elevation,
    hfi)

names(environmental_rasters) <-
  c(
    'temperature',
    'precipitation',
    'elevation',
    'hfi')

plot(environmental_rasters)
plot(environmental_rasters$hfi)

# background --------------------------------------------------------------

environment <-
  as.data.frame(
    environmental_rasters,
    na.rm = FALSE) %>%
  as_tibble() %>%
  mutate(
    type = 'background')

# background --------------------------------------------------------------

environment <-
  as.data.frame(
    environmental_rasters,
    na.rm = FALSE) %>%
  as_tibble() %>%
  mutate(status = 'background')

# occurrences -------------------------------------------------------------

environment_occs <-
  terra::extract(
    environmental_rasters,
    occs %>%
      select(x, y),
    ID = FALSE) %>%
  as_tibble()

occs_niche <-
  bind_cols(
    occs,
    environment_occs) %>%
  mutate(
    type = 'occurrence',
    .before = x)

# check missing values ----------------------------------------------------

occs_niche %>%
  filter(
    if_any(
      c(
        temperature,
        precipitation,
        elevation,
        hfi),
      is.na))

# full environment --------------------------------------------------------

environment <-
  environment %>%
  bind_rows(
    occs_niche %>%
      select(
        temperature,
        precipitation,
        elevation,
        hfi,
        status))

# temperature and precipitation ------------------------------------------

env_temp_prec <-
  environment %>%
  filter(
    !is.na(temperature),
    !is.na(precipitation)) %>%
  ggplot(
    aes(
      x = temperature,
      y = precipitation,
      col = status,
      shape = status)) +
  geom_point(size = 5) +
  scale_color_manual(
    values = c('gray30', 'blue', 'orange'),
    name = '',
    labels = c(
      'Background',
      'Before 2000',
      'Since 2000')) +
  scale_shape_manual(
    values = c(19, 21, 21)) +
  scale_x_continuous(
    name = 'Mean annual temperature (C)') +
  scale_y_continuous(
    name = 'Mean annual precipitation (mm)') +
  guides(shape = 'none') +
  theme_classic() +
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12))

env_temp_prec

# temperature and hfi -----------------------------------------------------

env_elev_hfi <-
  environment %>%
  filter(
    !is.na(elevation),
    !is.na(hfi)) %>%
  ggplot(
    aes(
      x = elevation,
      y = hfi,
      col = status,
      shape = status)) +
  geom_point(size = 5) +
  scale_color_manual(
    values = c('gray30', 'blue', 'orange'),
    name = '',
    labels = c(
      'Background',
      'Before 2000',
      'After 2000')) +
  scale_shape_manual(
    values = c(19, 21, 21)) +
  scale_x_continuous(
    name = 'Human footprint') +
  scale_y_continuous(
    name = 'Elevation (m)') +
  guides(shape = 'none') +
  theme_classic() +
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12))

env_elev_hfi

# temperature and elevation ----------------------------------------------

env_temp_elev <-
  environment %>%
  filter(
    !is.na(temperature),
    !is.na(elevation)) %>%
  ggplot(
    aes(
      x = temperature,
      y = elevation,
      col = status,
      shape = status)) +
  geom_point(size = 5) +
  scale_color_manual(
    values = c('gray30', 'blue', 'orange'),
    name = '',
    labels = c(
      'Background',
      'Since 2000',
      'Before 2000')) +
  scale_shape_manual(
    values = c(19, 21, 21)) +
  scale_x_continuous(
    name = 'Mean annual temperature (C)') +
  scale_y_continuous(
    name = 'Elevation (m)') +
  guides(shape = 'none') +
  theme_classic() +
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12))

env_temp_elev

# save figures ------------------------------------------------------------

ggsave(
  env_temp_prec,
  filename =
    paste0(
      'output/figures/',
      my_species,
      '_env_temp_prec.jpg'),
  dpi = 300)

ggsave(
  env_elev_hfi,
  filename =
    paste0(
      'output/figures/',
      my_species,
      '_env_elev_hfi.jpg'),
  dpi = 300)

ggsave(
  env_temp_elev,
  filename =
    paste0(
      'output/figures/',
      my_species,
      '_env_temp_elev.jpg'),
  dpi = 300)

# map ---------------------------------------------------------------------

elevation_map <-
  tm_shape(elevation) +
  tm_graticules(lines = FALSE) +
  tm_raster(
    col.scale =
      tm_scale_continuous(
        values = terrain.colors(9)),
    col.legend =
      tm_legend(
        'Elevation (m)',
        orientation = 'landscape',
        width = 60,
        title.size = 2,
        frame = FALSE)) +
  tm_shape(costa_rica) +
  tm_borders() +
  tm_shape(santa_elena) +
  tm_borders(col = 'darkgreen') +
  tm_shape(
    occs_sf %>%
      filter(status == 'before_2000')) +
  tm_dots(
    col = 'red',
    shape = 21,
    size = 0.3) +
  tm_shape(
    occs_sf %>%
      filter(status == 'since_2000')) +
  tm_dots(
    col = 'black',
    shape = 21,
    size = 0.3) +
  tm_scalebar(
    breaks = c(0, 50, 100),
    position = c('left', 'bottom')) +
  tm_layout(
    legend.outside = TRUE)

elevation_map

tmap_save(
  elevation_map,
  paste0(
    'output/figures/',
    my_species,
    '_elevation.jpg'),
  dpi = 300)

