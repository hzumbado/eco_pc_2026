# Ecologia de poblaciones
# Profesor Hector Zumbado Ulate
# Tablas de vida I

# setup -------------------------------------------------------------------

rm(list = ls())

library(tidyverse)
library(gt)

# data --------------------------------------------------------------------

heredia <-
  read_csv('data/raw/datos_heredia.csv') %>%
  mutate(
    sitio = 'Heredia',
    .after = grupo) # 1403 datos

san_jose <-
  read_csv('data/raw/datos_san_jose.csv') %>%
  mutate(
    sitio = 'San José',
    .after = grupo) # 1360 datos

cementerio <-
  bind_rows(
    heredia,
    san_jose)

# edad de muerte ----------------------------------------------------------

datos <-
  cementerio %>%
  mutate(
    edad_muerte = muerte - nacimiento)

# revision de datos -------------------------------------------------------

datos_revision <-
  datos %>%
  filter(
    edad_muerte < 0 |
      edad_muerte > 110)

datos_revision

# Se excluyen temporalmente los registros con edades de muerte
# menores que 0 o mayores que 110 anos.
# Estos registros deben revisarse en los datos originales.

datos <-
  datos %>%
  filter(
    edad_muerte >= 0,
    edad_muerte <= 110)

# clases de edad ----------------------------------------------------------

datos <-
  datos %>%
  mutate(
    edad = case_when(
      edad_muerte <= 1 ~ '0-01',
      edad_muerte <= 5 ~ '02-05',
      edad_muerte <= 10 ~ '06-10',
      edad_muerte <= 15 ~ '11-15',
      edad_muerte <= 20 ~ '16-20',
      edad_muerte <= 25 ~ '21-25',
      edad_muerte <= 30 ~ '26-30',
      edad_muerte <= 35 ~ '31-35',
      edad_muerte <= 40 ~ '36-40',
      edad_muerte <= 45 ~ '41-45',
      edad_muerte <= 50 ~ '46-50',
      edad_muerte <= 55 ~ '51-55',
      edad_muerte <= 60 ~ '56-60',
      edad_muerte <= 65 ~ '61-65',
      edad_muerte <= 70 ~ '66-70',
      edad_muerte <= 75 ~ '71-75',
      edad_muerte <= 80 ~ '76-80',
      edad_muerte <= 85 ~ '81-85',
      edad_muerte <= 90 ~ '86-90',
      edad_muerte <= 95 ~ '91-95',
      TRUE ~ '96+'),
    edad = factor(
      edad,
      levels = c(
        '0-01', '02-05', '06-10', '11-15',
        '16-20', '21-25', '26-30', '31-35',
        '36-40', '41-45', '46-50', '51-55',
        '56-60', '61-65', '66-70', '71-75',
        '76-80', '81-85', '86-90', '91-95',
        '96+')))

# resumen -----------------------------------------------------------------

datos %>%
  summarize(
    n = n(),
    .by = c(
      sitio,
      sexo))

# frecuencias de mortalidad -----------------------------------------------

frec <-
  datos %>%
  count(
    sitio,
    edad,
    sexo,
    .drop = FALSE) %>%
  pivot_wider(
    names_from = sexo,
    values_from = n,
    values_fill = 0) %>%
  mutate(
    total = femenino + masculino)

frec

# tabla de vida -----------------------------------------------------------

life_table <-
  frec %>%
  group_by(
    sitio) %>%
  mutate(
    intervalo = row_number(),
    nxf = rev(cumsum(rev(femenino))),
    nxm = rev(cumsum(rev(masculino))),
    nxt = rev(cumsum(rev(total))),
    lxf = nxf / first(nxf),
    lxm = nxm / first(nxm),
    lxt = nxt / first(nxt),
    qxf = femenino / nxf,
    qxm = masculino / nxm,
    qxt = total / nxt,
    logf = log10(lxf),
    logm = log10(lxm),
    logt = log10(lxt),
    .after = edad) %>%
  ungroup()

life_table

# cuadro ------------------------------------------------------------------

cuadro_vida <-
  life_table %>%
  select(
    sitio,
    edad,
    total,
    nxt,
    lxt,
    qxt) %>%
  gt(
    groupname_col = 'sitio') %>%
  cols_label(
    edad = 'Intervalo de edad',
    total = 'd\u2093',
    nxt = 'n\u2093',
    lxt = 'l\u2093',
    qxt = 'q\u2093') %>%
  fmt_number(
    columns = c(
      lxt,
      qxt),
    decimals = 3) %>%
  tab_options(
    table.font.size = px(12))

cuadro_vida

# output ------------------------------------------------------------------

life_table %>%
  write_csv('data/processed/life_table.csv')

datos %>%
  write_csv('data/processed/datos_cementerios.csv')

datos_revision %>%
  write_csv('data/processed/datos_revision.csv')

cuadro_vida %>%
  gtsave(
    filename = 'cuadro_vida.docx',
    path = 'output/tables')
