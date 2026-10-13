# Ecologia de poblaciones y comunidades
# Profesor Hector Zumbado Ulate
# descarga de datos gbif. Una especie

# setup -------------------------------------------------------------------

rm(list = ls())

library(rgbif)
library(tidyverse)

# data --------------------------------------------------------------------

species <- 'Craugastor fitzingeri'
my_species <- 'craugastor_fitzingeri'

# taxon key ---------------------------------------------------------------

key <-
  name_backbone(
    name = species) %>%
  pull(usageKey)

key

# download request --------------------------------------------------------

gbif_download <-
  occ_download(
    pred(
      'taxonKey',
      key),
    format = 'SIMPLE_CSV')

gbif_download

# save download information ----------------------------------------------

gbif_download %>%
  write_rds(
    paste0(
      'data/raw/',
      my_species,
      '_gbif_download.rds'))

# read download information ----------------------------------------------

gbif_download <-
  read_rds(
    paste0(
      'data/raw/',
      my_species,
      '_gbif_download.rds'))

# check download processing -----------------------------------------------

occ_download_wait(
  gbif_download)

# import data -------------------------------------------------------------

data <-
  occ_download_get(
    gbif_download,
    path = 'data/raw',
    overwrite = TRUE) %>%
  occ_download_import() %>%
  as_tibble()

# check -------------------------------------------------------------------

nrow(data)

data %>%
  distinct(species)

# save data ---------------------------------------------------------------

# rds

data %>%
  write_rds(
    paste0(
      'data/raw/',
      my_species,
      '_gbif_raw.rds'))

# check saved files -------------------------------------------------------

read_rds(
  paste0(
    'data/raw/',
    my_species,
    '_gbif_raw.rds'))
