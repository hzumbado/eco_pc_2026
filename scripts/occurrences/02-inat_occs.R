# Ecologia de poblaciones y comunidades
# Profesor Hector Zumbado Ulate
# descarga de datos iNaturalist. Una especie

# setup -------------------------------------------------------------------

rm(list = ls())

library(rinat)
library(tidyverse)

# species -----------------------------------------------------------------

query <- 'Craugastor fitzingeri'
my_species <- 'craugastor_fitzingeri'


# metadata ----------------------------------------------------------------

inat_metadata <-
  get_inat_obs(
    query = query,
    meta = TRUE) %>%
  pluck('meta')

inat_metadata

# download data -----------------------------------------------------------

inat_data <-
  get_inat_obs(
    query = query,
    quality = 'research',
    geo = TRUE,
    maxresults = 10000,
    meta = FALSE) %>%
  as_tibble()

# check -------------------------------------------------------------------

nrow(inat_data)

inat_data %>%
  distinct(scientific_name)

inat_data %>%
  count(
    scientific_name,
    sort = TRUE)

inat_occurrences <-
  inat_data %>%
  filter(
    scientific_name == query)

# save data ---------------------------------------------------------------

inat_occurrences %>%
  write_rds(
    paste0(
      'data/raw/',
      my_species,
      '_inat_raw.rds'))

# check saved files -------------------------------------------------------

read_rds(
  paste0(
    'data/raw/',
    my_species,
    '_inat_raw.rds'))
