# Ecologia de poblaciones y comunidades
# Profesor Hector Zumbado Ulate
# limpieza de datos de ocurrencia
# GBIF + iNaturalist. Una especie

# setup -------------------------------------------------------------------

rm(list = ls())

library(CoordinateCleaner)
library(sf)
library(tmap)
library(tidyverse)

# species -----------------------------------------------------------------

species <- 'Craugastor fitzingeri'
my_species <- 'craugastor_fitzingeri'

# shapefiles --------------------------------------------------------------

list.files(
  'shapefiles',
  pattern = '(cos|crp)',
  full.names = TRUE) %>%
  map(
    ~.x %>%
      read_sf() %>%
      st_make_valid()) %>%
  set_names(
    'costa_rica',
    'crpn') %>%
  list2env(.GlobalEnv)

# inaturalist -------------------------------------------------------------

inat_occurrences <-
  read_rds(
    paste0(
      'data/raw/',
      my_species,
      '_inat_raw.rds')) %>%
  as_tibble() %>%
  mutate(
    species = scientific_name,
    x = longitude,
    y = latitude,
    date_original = observed_on,
    date = ymd(observed_on),
    date_precision = 'day',
    accuracy = positional_accuracy,
    obscured =
      tolower(
        as.character(
          coordinates_obscured)) == 'true',
    source = 'iNaturalist',
    source_id = as.character(id),
    id = paste0(
      'inat_',
      id)) %>%
  select(
    species,
    x,
    y,
    date_original,
    date,
    date_precision,
    accuracy,
    obscured,
    source,
    source_id,
    id) %>%
  filter(
    species == species_name) %>%
  filter(
    !is.na(x),
    !is.na(y),
    is.finite(x),
    is.finite(y)) %>%
  filter(
    x >= -180,
    x <= 180,
    y >= -90,
    y <= 90) %>%
  distinct(
    id,
    .keep_all = TRUE)

# check obscured observations ---------------------------------------------

inat_occurrences %>%
  count(
    obscured)

# gbif --------------------------------------------------------------------

gbif_occurrences <-
  read_rds(
    paste0(
      'data/raw/',
      my_species,
      '_gbif_raw.rds')) %>%
  as_tibble() %>%
  filter(
    species == species_name) %>%
  mutate(
    date_precision =
      case_when(
        !is.na(day) ~ 'day',
        !is.na(month) ~ 'month',
        !is.na(year) ~ 'year',
        TRUE ~ NA_character_),
    date_original = eventDate,
    month_clean =
      replace_na(
        month,
        1),
    day_clean =
      replace_na(
        day,
        1),
    date =
      make_date(
        year,
        month_clean,
        day_clean),
    source = 'GBIF',
    source_id = as.character(gbifID),
    id = paste0(
      'gbif_',
      gbifID),
    obscured = FALSE) %>%
  filter(
    is.na(institutionCode) |
      institutionCode != 'iNaturalist') %>%
  select(
    species,
    x = decimalLongitude,
    y = decimalLatitude,
    date_original,
    date,
    date_precision,
    accuracy = coordinateUncertaintyInMeters,
    obscured,
    source,
    source_id,
    id) %>%
  filter(
    !is.na(x),
    !is.na(y),
    is.finite(x),
    is.finite(y)) %>%
  filter(
    x >= -180,
    x <= 180,
    y >= -90,
    y <= 90) %>%
  distinct(
    id,
    .keep_all = TRUE)

# combine datasets --------------------------------------------------------

occurrences <-
  bind_rows(
    inat_occurrences,
    gbif_occurrences)

# temporal filter ---------------------------------------------------------

occurrences <-
  occurrences %>%
  filter(
    !is.na(date)) %>%
  filter(
    year(date) >= 1950)

# coordinate quality ------------------------------------------------------

# CoordinateCleaner is used to flag potentially problematic coordinates.
# Records are not removed at this stage.
# The 'seas' test is excluded because occurrences on small islands and
# complex coastlines can produce false positives.

occurrences <-
  occurrences %>%
  clean_coordinates(
    lon = 'x',
    lat = 'y',
    species = 'species',
    tests = c(
      'capitals',
      'centroids',
      'equal',
      'gbif',
      'institutions',
      'zeros'),
    value = 'spatialvalid') %>%
  as_tibble()

# coordinate cleaner summary ---------------------------------------------

# TRUE = passed all CoordinateCleaner tests
# FALSE = flagged by at least one test
# FALSE does not necessarily mean that the record is incorrect

occurrences <-
  occurrences %>%
  rename(
    cc_passed_all = .summary)

# repeated coordinates ---------------------------------------------------

# Repeated coordinates are identified but not removed.
# Multiple observations can legitimately share the same coordinates.

occurrences <-
  occurrences %>%
  add_count(
    x,
    y,
    name = 'n_coordinate') %>%
  add_count(
    x,
    y,
    date,
    name = 'n_coordinate_date') %>%
  mutate(
    repeated_coordinate =
      n_coordinate > 1,
    repeated_coordinate_date =
      n_coordinate_date > 1)

# spatial object ----------------------------------------------------------

occurrences_sf <-
  occurrences %>%
  st_as_sf(
    coords = c(
      'x',
      'y'),
    crs = 4326,
    remove = FALSE)

# check dataset -----------------------------------------------------------

nrow(occurrences)
nrow(occurrences_sf)

# number of observations by source

occurrences_sf %>%
  st_drop_geometry() %>%
  count(
    source)

# obscured iNaturalist observations

occurrences_sf %>%
  st_drop_geometry() %>%
  count(
    source,
    obscured)

# temporal precision

occurrences_sf %>%
  st_drop_geometry() %>%
  count(
    date_precision)

# CoordinateCleaner general result

occurrences_sf %>%
  st_drop_geometry() %>%
  count(
    cc_passed_all)

# number of records flagged by each CoordinateCleaner test

occurrences_sf %>%
  st_drop_geometry() %>%
  select(
    starts_with('.')) %>%
  summarise(
    across(
      everything(),
      ~sum(
        .x == FALSE,
        na.rm = TRUE)))

# repeated coordinates

occurrences_sf %>%
  st_drop_geometry() %>%
  count(
    repeated_coordinate)

occurrences_sf %>%
  st_drop_geometry() %>%
  count(
    repeated_coordinate_date)

# costa rica --------------------------------------------------------------

occurrences_cr <-
  occurrences_sf %>%
  st_filter(
    costa_rica)

nrow(occurrences_cr)

occurrences_cr %>%
  st_drop_geometry() %>%
  count(
    source)

# map ---------------------------------------------------------------------

st_crs(costa_rica) <- 4326
st_crs(crpn) <- 4326

extent <-
  st_bbox(
    c(
      xmin = -86,
      xmax = -82,
      ymin = 8,
      ymax = 11.3),
    crs = st_crs(4326)) %>%
  st_as_sfc()

occs_cr <-
  crpn %>%
  tm_shape(
    bb = extent) +
  tm_polygons(
    'gray70') +
  tm_crs(4326) +
  tm_graticules(
    lines = FALSE) +
  tm_shape(
    occurrences_cr) +
  tm_dots(
    fill = 'source',
    size = 0.35,
    shape = 21,
    fill.legend =
      tm_legend(
        title = 'Source')) +
  tm_scalebar(
    breaks = c(
      0,
      50,
      100),
    position = c(
      'bottom',
      'left')) +
  tm_compass(
    position = c(
      'top',
      'right')) +
  tm_layout(
    bg.color = 'lightblue') +
  tm_crs(4326)

occs_cr

# save map ---------------------------------------------------------------

tmap_save(
  occs_cr,
  paste0(
    'output/figures/',
    my_species,
    '_occurrences.jpg'),
  dpi = 300)

# save clean datasets -----------------------------------------------------

# complete geographic dataset

occurrences %>%
  write_rds(
    paste0(
      'data/processed/',
      my_species,
      '_occ_clean.rds'))

# costa rica only

occurrences_cr %>%
  st_drop_geometry() %>%
  write_rds(
    paste0(
      'data/processed/',
      my_species,
      '_occ_clean_cr.rds'))
