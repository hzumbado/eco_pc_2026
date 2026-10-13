# Ecologia de poblaciones y comunidades
# Profesor Hector Zumbado Ulate
# Distribucion geografica y administrativa

# Setup -------------------------------------------------------------------

rm(list = ls())

library(sf)
library(tmap)
library(tidyverse)

# Shapefiles --------------------------------------------------------------

list.files(
  'shapefiles',
  pattern = '(cos|prov|can)',
  full.names = TRUE) %>%
  map(
    ~ .x %>%
      read_sf() %>%
      st_make_valid() %>%
      st_transform(4326) %>%
      janitor::clean_names()) %>%
  set_names(
    'costa_rica',
    'cantones',
    'provincias',
    'savage') %>%
  list2env(.GlobalEnv)

print(costa_rica)
print(cantones)
print(provincias)
print(savage)

# data --------------------------------------------------------------------

my_species <- 'craugastor_fitzingeri'

my_species_cr <-
  read_rds(
    paste0(
      'data/processed/',
      my_species,
      '_occ_canton.rds')) %>%
  st_as_sf(
    coords = c(
      'x',
      'y'),
    crs = 4326,
    remove = FALSE)

# Distribución administrativa ----------------------------------------------------------

my_species_sf <-
  my_species_cr %>%
  st_join(cantones) %>%
  select(
    species:date,
    canton,
    provincia,
    cod,
    everything())

# Distribucion provincial --------------------------------------------------

provincias_count <-
  my_species_sf %>%
  as_tibble() %>%
  summarize(
    n = n(),
    .by = provincia) %>%
  full_join(
    cantones,
    .,
    by = 'provincia')

provincias_count

provincias_map <-
  costa_rica %>%
  tm_shape() +
  tm_borders() +
  tm_shape(provincias_count) +
  tm_polygons(
    fill = 'n',
    fill.scale =
      tm_scale_continuous(
        values = 'viridis',
        label.na = 'No registrado'),
    fill.legend =
      tm_legend(
        'Observaciones'),
    col = NULL) +
  tm_shape(my_species_sf) +
  tm_symbols(
    size = 0.25,
    fill = 'black') +
  tm_shape(provincias) +
  tm_borders(
    lwd = 1) +
  tm_compass(
    position = tm_pos_in(
      'left',
      'bottom')) +
  tm_scalebar(
    breaks = c(
      0,
      10,
      50),
    position =
      tm_pos_in(
        'left',
        'bottom'))

provincias_map

tmap_save(
  provincias_map,
  paste0(
    'output/figures/',
    my_species,
    '_provincias.jpg'),
  dpi = 300)

# Distribucion cantonal --------------------------------------------------

cantones_count <-
  my_species_sf %>%
  as_tibble() %>%
  summarize(
    n = n(),
    .by = canton) %>%
  full_join(
    cantones,
    .,
    by = 'canton')

cantones_count

cantones_map <-
  costa_rica %>%
  tm_shape() +
  tm_borders() +
  tm_shape(cantones_count) +
  tm_polygons(
    fill = 'n',
    fill.scale =
      tm_scale_continuous(
        values = 'viridis',
        label.na = 'No registrado'),
    fill.legend =
      tm_legend(
        'Observaciones'),
    col = NULL) +
  tm_shape(my_species_sf) +
  tm_symbols(
    size = 0.25,
    fill = 'black') +
  tm_shape(cantones) +
  tm_borders(
    lwd = 1) +
  tm_compass(
    position = tm_pos_in(
      'left',
      'bottom')) +
  tm_scalebar(
    breaks = c(
      0,
      10,
      50),
    position =
      tm_pos_in(
      'left',
      'bottom'))

cantones_map

tmap_save(
  cantones_map,
  paste0(
    'output/figures/',
    my_species,
    '_cantones.jpg'),
  dpi = 300)

tmap_mode('view')

cantones_map

tmap_mode('plot')

# # mapa en ggplot
#
# extent <-
#   st_bbox( #crear extent con paquete sf
#     c(
#       xmin = -86,
#       xmax = -82.5,
#       ymin = 7.5,
#       ymax = 11.5),
#     crs = 4326)
#
# cantones_count %>%
#   ggplot() +
#   geom_sf(
#     aes(fill = n)) +
#   scale_fill_viridis_c(
#     option = 'plasma',
#     na.value = '#dcdcdc') +
#   coord_sf(
#     xlim = c(extent$xmin, extent$xmax),
#     ylim = c(extent$ymin, extent$ymax)) +
#   theme_classic() #mismo mapa en ggplot.

# distribucion en zona de vida --------------------------------------------

savage_regions <-
  savage %>%
  group_by(
    nprovincia) %>%
  summarize()

savage_regions

my_species_sf_savage <-
  my_species_cr %>%
  st_join(savage_regions) %>%
  select(
    species:date,
    nprovincia,
    everything())

savage_count <-
  my_species_sf_savage %>%
  st_drop_geometry() %>%
  count(
    nprovincia,
    name = 'n') %>%
  right_join(
    savage_regions,
    .,
    by = 'nprovincia')

savage_count

# ocurrencias costeras no asignadas ----------------------------

missing_savage <-
  my_species_sf_savage %>%
  filter(
    is.na(nprovincia))

missing_savage

savage %>%
  tm_shape() +
  tm_polygons() +
  tm_shape(
    missing_savage) +
  tm_dots(
    fill = 'red',
    size = 0.5)

# corregir ocurrencias costeras no asignadas ------------------------------

my_species_sf_savage <-
  my_species_sf_savage %>%
  filter(
    !is.na(nprovincia)) %>%
  bind_rows(
    my_species_sf_savage %>%
      filter(
        is.na(nprovincia)) %>%
      select(
        -nprovincia) %>%
      st_join(
        savage %>%
          select(nprovincia),
        join = st_nearest_feature))


my_species_sf_savage %>%
  st_drop_geometry() %>%
  count(
    nprovincia,
    name = 'n') %>%
  right_join(
    savage_regions,
    .,
    by = 'nprovincia')

# mapa provincias de Savage -----------------------------------------------

savage_map <-
  costa_rica %>%
  tm_shape() +
  tm_borders() +
  tm_shape(savage_count) +
  tm_polygons(
    fill = 'n',
    fill.scale =
      tm_scale_continuous(
        values = 'viridis',
        label.na = 'No registrado'),
    fill.legend =
      tm_legend(
        'Observaciones'),
    col = NULL) +
  tm_shape(my_species_sf_savage) +
  tm_symbols(
    size = 0.25,
    fill = 'black') +
  tm_shape(savage) +
  tm_borders(
    lwd = 1) +
  tm_compass(
    position = tm_pos_in(
      'left',
      'bottom')) +
  tm_scalebar(
    breaks = c(
      0,
      10,
      50),
    position =
      tm_pos_in(
        'left',
        'bottom'))

savage_map

tmap_save(
  savage_map,
  paste0(
    'output/figures/',
    my_species,
    '_savage.jpg'),
  dpi = 300)

# Distribucion historica --------------------------------------------------

county_history <-
  my_species_sf %>%
  st_drop_geometry() %>%
  group_by(canton) %>%
  summarize(
    n = n()) %>%
  slice_max(
    n,
    n = 20) %>%
  ggplot(
    aes(x = reorder(canton, n),
        y = n)) +
  geom_bar(
    stat = 'identity',
    fill = 'cornflowerblue',
    col = 'black') +
  coord_flip() +
  scale_y_continuous(
    expand = c(0, 0),
    breaks = c(0, 400)) +
  labs(
    x = 'Cantón',
    y = 'Observaciones') +
  theme_classic()

county_history

ggsave(
  paste0(
    'output/figures/',
    my_species,
    '_observation_cantones.jpg'),
  dpi = 300)

# Distribucion temporal --------------------------------------------------

dist_temp <-
  my_species_sf %>%
  group_by(year = year(date)) %>%
  summarize(n = n()) %>%
  ggplot(
    aes(year, n)) +
  geom_bar(
    stat = 'identity',
    col = 'black',
    fill = 'cornflowerblue') +
  scale_y_continuous(
    expand = c(0,0)) +
  scale_x_continuous(
    limits = c(1950, 2026)) +
  labs(
    x = 'Año',
    y = 'Observaciones desde 1950') +
  theme_classic()

dist_temp

ggsave(
  paste0(
    'output/figures/',
    my_species,
    '_observation_year.jpg'),
  dpi = 300)


# Distribucion desde el 2016 ----------------------------------------------

dist_temp2 <-
  my_species_sf %>%
  st_drop_geometry() %>%
  filter(year(date) %in% 2016:2026) %>%
  group_by(canton) %>%
  summarize(n = n()) %>%
  slice_max(n, n = 10) %>%
  ggplot(
    aes(x = reorder(canton, n),
        y = n)) +
  geom_bar(
    stat = 'identity',
    fill = 'cornflowerblue',
    colour = 'black') +
  coord_flip() +
  scale_y_continuous(expand = c(0, 0)) +
  labs(
    title = paste('Observaciones de Craugastor fitzingeri por cantón desde 2016'),
    x = 'Cantón',
    y = 'Observaciones') +
  theme_classic()

dist_temp2

ggsave(
  paste0(
    'output/figures/',
    my_species,
    '_observation_year2.jpg'),
  dpi = 300)

# Distribucion latitudinal historica --------------------------------------------------

lat <-
  my_species_sf %>%
  st_drop_geometry() %>%
  group_by(year = year(date)) %>%
  summarize(
    sur = min(y),
    norte = max(y)) %>%
  pivot_longer(
    sur:norte,
    names_to = 'limites',
    values_to = 'latitud') %>%
  ggplot(
    aes(x = year,
        y = latitud)) +
  geom_point() +
  geom_line(aes(col = limites)) +
  scale_color_manual(
    values = c('blue', 'orange'),
    name = 'Límites',
    labels = c('Norte', 'Sur')) +
  labs(
    x = 'Año',
    y = 'Latitud') +
  theme_classic() +
  theme(
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 16),
    legend.position.inside = c(.5, .3),
    legend.background = element_rect(
      fill = 'gray90',
      linewidth =  0.5,
      linetype = 'solid',
      colour = 'gray20'))

lat

ggsave(
  paste0(
    'output/figures/',
    my_species,
    '_observation_latitude.jpg'),
  dpi = 300)

# Distribucion longitudinal historica --------------------------------------------------

long <-
  my_species_sf %>%
  st_drop_geometry() %>%
  group_by(year = year(date)) %>%
  summarize(
    oeste = min(x),
    este = max(x)) %>%
  pivot_longer(
    oeste:este,
    names_to = 'limites',
    values_to = 'longitud') %>%
  ggplot(
    aes(
      x = year,
      y = longitud)) +
  geom_point() +
  geom_line(aes(col = limites)) +
  scale_color_manual(
    values = c('blue', 'orange'),
    name = 'Límites',
    labels = c('Oeste', 'Este')) +
  labs(
    x = 'Año',
    y = 'Longitud') +
  theme_classic() +
  theme(
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 16),
    legend.position.inside = c(.5, .3),
    legend.background = element_rect(
      fill = 'gray90',
      linewidth =  0.5,
      linetype = 'solid',
      colour = 'gray20'))

long

ggsave(
  paste0(
    'output/figures/',
    my_species,
    '_observation_longitude.jpg'),
  dpi = 300)

longlat <-
  my_species_sf %>%
  st_drop_geometry() %>%
  filter(
    x >= -86,
    x <= -82,
    y >= 8,
    y <= 12) %>%
  pivot_longer(
    c(x, y),
    names_to = 'axis',
    values_to = 'coordinates') %>%
  mutate(
    axis = factor(
      axis,
      levels = c('x', 'y'))) %>%
  relocate(
    axis:coordinates,
    .after = species) %>%
  ggplot(aes(x = coordinates)) +
  geom_density(fill = 'salmon') +
  facet_wrap(
    ~ axis,
    scales = 'free') +
  scale_y_continuous(expand = c(0, 0)) +
  labs(
    title = 'Densidad de observaciones por longitud y latitud',
    x = 'Coordenadas',
    y = 'Densidad') +
  theme_classic()

longlat

ggsave(
  paste0(
    'output/figures/',
    my_species,
    '_density_long-lat.jpg'),
  dpi = 300)

# Observación histórica por cantón ---------------------------------------

first_observation <-
  my_species_sf %>%
  st_drop_geometry() %>%
  mutate(
    year = year(date)) %>%
  summarize(
    year = min(year),
    .by = canton) %>%
  ggplot(
    aes(
      x = reorder(
        canton,
        desc(year)),
      y = year)) +
  geom_segment(
    aes(
      xend = canton,
      y = 2026,
      yend = year),
    color = '#6f8faf') +
  geom_point(
    colour = 'black',
    fill = 'cornflowerblue',
    size = 2,
    shape = 21) +
  coord_flip() +
  labs(
    x = 'Cantón',
    y = 'Año') +
  theme_classic()

first_observation

ggsave(
  paste0(
    'output/figures/',
    my_species,
    '_first_observation.jpg'),
  dpi = 300)


all_observations <-
  my_species_sf %>%
  st_drop_geometry() %>%
  group_by(canton) %>%
  mutate(year = year(date)) %>%
  ggplot(
    aes(
      x = reorder(canton, desc(year)),
      y = year)) +
  geom_segment(
    aes(
      x = reorder(canton, desc(year)),
      xend = canton,
      y = 2026,
      yend = year),
    color = 'cornflowerblue') +
  geom_point(
    colour = 'black',
    fill = 'cornflowerblue',
    size = 2,
    shape = 21) +
  coord_flip() +
  labs(
      x = 'Cantón',
    y = 'Año') +
  theme_classic()

all_observations

ggsave(
  paste0(
    'output/figures/',
    my_species,
    '_all_observations.jpg'),
  dpi = 300)
