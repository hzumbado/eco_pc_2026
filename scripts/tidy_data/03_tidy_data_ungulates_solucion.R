# Ecologia de poblaciones y comunidades
# Lectura de datos ordenadas
# Profesor Hector Zumbado Ulate

# setup -------------------------------------------------------------------

rm(list = ls())

library(gt)
library(tidyverse)

# data ------------------------------------------

angola_ungulates <-
  read_csv('data/raw/angola_ungulates.csv')

lobstr::ref(.GlobalEnv)

# 1- Arregle la fecha creando una columna fecha con las funciones unite() y as_date(). La fecha debe estar ordenada como year, month, day. Asigne al objeto resultante el nombre angola_ungulates_date_fix

angola_ungulates_date_fix <-
  angola_ungulates %>%
  unite(
    date,
    year:day,
    sep = '-') %>%
  mutate(date = as_date(date))

# 2- Haga una nueva columna con el nombre cientifico (sci_name) con la informacion de las columnas llamadas 'genus' y 'species' utilizando la funcion unite. Como argumento separador use sep = ' ' de manera que quede un espacio entre el genero y la especie. Luego utilice la funcion rename para cambiar la columna sci_name a 'species'. Asigne al objeto resultante el nombre angola_ungulates_spp_fix.

angola_ungulates_spp_fix <-
  angola_ungulates_date_fix %>%
  unite(
    sci_name,
    c(genus,species),
    sep = ' ') %>%
  rename(species = sci_name)

# 3- Separe la columna taxonomia en tres columnas llamadas 'class', 'order', 'family utilizando la funcion separate. Como argumento separador use sep = '-' de manera que R entienda que cada guion separa un nombre correspondiente a cada columna.

angola_ungulates_taxonomy_fix <-
  angola_ungulates_spp_fix %>%
  separate(
    taxonomy,
    into = c('class', 'order', 'family'),
    sep = '-')

# haga una lista con 2 elementos llamados 'taxonomy' y 'observations'. Para el objeto taxonomy utilice las columnas species, order, family, common_name. Para el objeto observations seleccione date, user_login, species).  Como el dato Class es el mismo para ambos ordenes (Mammalia) es innecesario ponerlo.

taxonomy <-
  select(
    angola_ungulates_taxonomy_fix,
    species,
    order:family,
    common_name) %>%
  distinct()

observations <-
  select(
    angola_ungulates_taxonomy_fix,
    date:user_login,
    species)

observations

list(
  taxonomy = taxonomy,
  observations = observations) %>%
  write_rds('data/processed/ungulates_list.rds')


# figure -------------------------------------------------------------------

# número de observaciones por especie

observations %>%
  count(species) %>%
  ggplot(
    aes(
      x = n,
      y = reorder(species, n))) +
  geom_col() +
  labs(
    x = 'Number of observations',
    y = NULL) +
  theme_classic() +
  theme(
    axis.text.y = element_text(face = 'italic'))

ggsave(
  'output/figures/ungulate_observations.tiff',
  width = 180,
  height = 120,
  units = 'mm',
  dpi = 300,
  compression = 'lzw',
  bg = 'white')


# cuadro ------------------------------------------------------------------

# taxonomia

taxonomy %>%
  mutate(
    species = paste0('*', species, '*')) %>%
  gt() %>%
  tab_header(
    title = md(
      '**Ungulate species recorded in Angola and their taxonomic classification.**')) %>%
  cols_label(
    species = 'Species',
    order = 'Order',
    family = 'Family',
    common_name = 'Common name') %>%
  fmt_markdown(
    columns = species) %>%
  opt_row_striping()
