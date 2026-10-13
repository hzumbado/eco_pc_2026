# Ecologia de poblaciones y comunidades
# Codigo 01a Lectura de datos ordenados
# Profesor Hector Zumbado Ulate

# setup ----------------------------------------------------------

rm(list = ls())

library(gt)
library(tidyverse)

# data -------------------------------------------------------------

read_csv('data/raw/messy_weather.csv') %>%
  view()

read_csv('data/raw/messy_weather.csv') %>%
  lobstr::ref()

# fixing data -------------------------------------------------

# Read in and assign the data:

messy_weather_long <-
  read_csv('data/raw/messy_weather.csv') %>%
  pivot_longer(
    cols = march_1:march_31,
    names_to = 'day',
    values_to = 'value',
    names_prefix = 'march_')

messy_weather_long %>%
  view()

unique(messy_weather_long$checked)

messy_weather_long %>%
  distinct(checked)

unique(messy_weather_long$state)

messy_weather_long %>%
  distinct(state)

unique(messy_weather_long$variable)

messy_weather_long %>%
  distinct(variable)

unique(messy_weather_long$Station)

messy_weather_long %>%
  distinct(Station)

unique(messy_weather_long$Name)

messy_weather_long %>%
  distinct(Name)


# fix names ---------------------------------------------------------------

messy_weather_long %>%
  select(
    station = Station,
    x = longitude,
    y,
    elevation,
    state,
    name = Name,
    year:value)

weather_names_fix <-
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
    year:value)

weather_names_fix %>%
  view()

# fix variables -----------------------------------------------------------

weather_names_fix %>%
  pivot_wider(
    names_from = 'variable',
    values_from = 'value')

weather_variable_fix <-
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
    names_from = 'variable',
    values_from = 'value') %>%
  rename(precipitation = precip)

weather_variable_fix %>%
  view()

# create date -------------------------------------------------------------

weather_variable_fix %>%
  unite(
    col = 'date', #new column
    year:day,
    sep = '-') %>%
  mutate(date = as_date(date))

weather_date_fix <-
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
  mutate(date = as_date(date))

weather_date_fix

# fix temp ----------------------------------------------------------------
names(weather_date_fix)

weather_date_fix %>%
  select(temperature_min_max)

weather_date_fix %>%
  select(temperature_min_max) %>%
  separate(
    temperature_min_max, #columna a separar
    into = c('temperature_min', 'temperature_max'),
    sep = ':')

weather_fix_temp <-
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
    sep = ':')

weather_fix_temp

# fix values --------------------------------------------------------------

weather_fix_temp %>%
  pivot_longer(
    cols = precipitation:temperature_max,
    names_to = 'variable',
    values_to = 'value') %>%
  filter(is.na(value))

weather_fix_temp %>%
  mutate(
    precipitation = as.numeric(precipitation),
    snow = as.numeric(snow),
    temperature_min = as.numeric(temperature_min),
    temperature_max = as.numeric(temperature_max))

# much better to use across

weather_fix_temp %>%
  mutate(
    across(
      precipitation:temperature_max,
      ~ as.numeric(.x)))

weather_value_temp <-
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

weather_value_temp

# output/results ----------------------------------------------------------

# grafico para compararar valores de Temperatura

weather_value_temp %>%
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

weather_value_temp %>%
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

# individual tables -------------------------------------------------------

# there are two different dataframes in this dataset
# station data
# weather data

unique(weather_value_temp$station)

weather_value_temp %>%
  distinct(
    station,
    x,
    y,
    elevation,
    state,
    name)

weather_value_temp %>%
  select(station:name) %>%
  distinct()

# Generate a station-level data frame and assign to the name `stations`:

stations <-
  weather_value_temp %>%
  select(station:name) %>%
  distinct()

stations

# Generate a observation-level data frame and assign to the name
# `observations`:

observations <-
  weather_value_temp %>%
  select(!x:name)

observations

stations %>%
  inner_join(observations)

# create a list to save ---------------------------------------------------

list(stations, observations)

list(
  stations =
    weather_value_temp %>%
    select(station:name) %>%
    distinct(),

  observations =
    weather_value_temp %>%
    select(!x:name)) %>%

  # Write to file:

  write_rds('data/processed/weather_tidy.rds')
