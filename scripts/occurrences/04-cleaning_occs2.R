# Ecologia de poblaciones y comunidades
# Profesor Hector Zumbado Ulate
# Preparacion de ocurrencias para analisis espaciales
# KDE, nicho bidimensional y ocurrencias por canton

# El siguiente script aplica una limpieza final y genera dos datasets: uno se usará con todas las ocurrencias válidas para el análisis posterior por cantón y otro para análisis espaciales finos (Kernel Density  y nicho bidmensional)

# setup -------------------------------------------------------------------

rm(list = ls())

library(sf)
library(terra)
library(tidyverse)

# species -----------------------------------------------------------------

species <- 'Craugastor fitzingeri'
my_species <- 'craugastor_fitzingeri'

# data --------------------------------------------------------------------

occurrences <-
  read_rds(
    paste0(
      'data/processed/',
      my_species,
      '_occ_clean_cr.rds'))

# inspect -----------------------------------------------------------------

nrow(occurrences)

occurrences %>%
    count(
    source,
    obscured)

occurrences %>%
  summarise(
    n = n(),
    repeated_coordinates =
      sum(
        repeated_coordinate),
    obscured =
      sum(
        obscured))

# coordinate quality ------------------------------------------------------

# Eliminar únicamente coordenadas claramente inválidas y centroides
# detectados por CoordinateCleaner.
# Se conservan los registros marcados como cercanos a capitales.

occ_cc <-
  occurrences %>%
  filter(
    .val == TRUE,
    .equ == TRUE,
    .zer == TRUE,
    .cen == TRUE) %>%
  select(
    species,
    x,
    y,
    date,
    source,
    accuracy,
    obscured)

# dataset para abundancia por cantón ---------------------------------------------

# todos los registros se mantienen incluyendo coordenadas repetidas

occ_canton <-
  occ_cc

# dataset for spatial analyses --------------------------------------------

# spatial dataset ---------------------------------------------------------

# Eliminar coordenadas inválidas, ceros, latitud-longitud iguales y
# centroides administrativos y coordenadas oscurecidas
# Las coordenadas repetidas se conservan en esta etapa.
# La depuración adicional se realizará por separado para los análisis de KDE y nicho ambiental.

occ_spatial <-
  occ_cc %>%
  filter(obscured == FALSE) %>%
  select(
    species,
    x,
    y,
    date,
    source,
    accuracy)

# check final datasets ----------------------------------------------------

nrow(occurrences)
nrow(occ_canton)
nrow(occ_spatial)

occ_canton %>%
  count(
    source)

occ_spatial %>%
  count(
    source)

# save --------------------------------------------------------------------

occ_canton %>%
  write_rds(
    paste0(
      'data/processed/',
      my_species,
      '_occ_canton.rds'))

occ_spatial %>%
  write_rds(
    paste0(
      'data/processed/',
      my_species,
      '_occ_spatial.rds'))
