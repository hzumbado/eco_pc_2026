# Ecologia de poblaciones y comunidades
# Codigo 01b. Lectura de datos ordenados
# Profesor Hector Zumbado Ulate

# setup ----------------------------------------------------------

rm(list = ls())

library(gt)
library(tidyverse)

# data -------------------------------------------------------------

weather_data <-
  read_csv('data/raw/messy_weather.csv') %>%
  pivot_longer(
    cols = march_1:march_31,
    names_to = 'day',
    values_to = 'value',
    names_prefix = 'march_') %>%
  select(
    station = Station,
    x = longitude,
    y,
    elevation,
    state,
    name = Name,
    year:value) %>%
  pivot_wider(
    names_from = variable) %>%
  rename(precipitation = precip) %>%
  unite(
    col = 'date',
    year:day,
    sep = '-') %>%
  mutate(date = as_date(date)) %>%
  separate(
    temperature_min_max,
    into = c('temperature_min', 'temperature_max'),
    sep = ':') %>%
  mutate(
    across(
      precipitation:temperature_max,
      ~ as.numeric(.x)))

# create a list to save ---------------------------------------------------

list(
  stations =
    weather_data %>%
    select(station:name) %>%
    distinct(),
  observations =
    weather_data %>%
    select(!x:name)) %>%
  write_rds('data/processed/weather_tidy.rds')

# extraer elementos pluck()

stations <-
  read_rds('data/processed/weather_tidy.rds') %>%
  pluck('stations')

observations <-
  read_rds('data/processed/weather_tidy.rds') %>%
  pluck('stations')

# con funcion list2()

rm(list = ls())

read_rds('data/processed/weather_tidy.rds') %>%
  list2env(.GlobalEnv)

stations %>%
  inner_join(observations) %>%
  group_by(name) %>%
  summarise(
    temperature_min =
      mean(temperature_min, na.rm = TRUE),
    temperature_max =
      mean(temperature_max, na.rm = TRUE)) %>%
  ggplot(
    aes(
      x = reorder(name, temperature_max),
      ymin = temperature_min,
      ymax = temperature_max
    )) +
  geom_linerange(linewidth = 1) +
  geom_point(
    aes(y = temperature_min),
    size = 3,
    shape = 21,
    fill = 'white') +
  geom_point(
    aes(y = temperature_max),
    size = 3) +
  coord_flip() +
  labs(
    x = NULL,
    y = 'Mean temperature (°C)') +
  theme_classic()

ggsave(
  'output/figures/temperature.tiff',
  width = 180,
  height = 120,
  units = 'mm',
  dpi = 300,
  compression = 'lzw',
  bg = 'white')

## cuadro para comparar valores de variables

stations %>%
  inner_join(observations) %>%
  group_by(name, state, elevation) %>%
  summarise(
    precipitation =
      mean(precipitation, na.rm = TRUE),
    snow =
      mean(snow, na.rm = TRUE),
    temperature_min =
      mean(temperature_min, na.rm = TRUE),
    temperature_max =
      mean(temperature_max, na.rm = TRUE)) %>%
  ungroup() %>%
  gt() %>%
  tab_header(
    title = md(
      '**Mean climatic conditions recorded at five weather stations during March (2010–2020).**'
    )) %>%
  cols_label(
    name = 'Station',
    state = 'State',
    elevation = 'Elevation (m)',
    precipitation = 'Precipitation (mm)',
    snow = 'Snow (mm)',
    temperature_min = 'Min. temp. (°C)',
    temperature_max = 'Max. temp. (°C)') %>%
  fmt_number(
    columns = elevation:temperature_max,
    decimals = 1) %>%
  opt_row_striping()
