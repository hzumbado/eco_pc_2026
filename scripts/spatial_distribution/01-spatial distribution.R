# Ecologia de poblaciones y comunidades
# Distribucion espacial de poblaciones
# El Rodeo, Ciudad Colon
# Profesor Hector Zumbado Ulate
#
# Objetivos:
# 1. Describir la abundancia de dos especies de plantas.
# 2. Evaluar su patron de distribucion espacial.
# 3. Comparar los patrones observados con distribuciones teoricas.

# Setup -------------------------------------------------------------------

rm(list = ls())

library(cowplot)
library(gt)
library(MASS)
library(tidyverse)


# Importar datos ----------------------------------------------------------

datos <-
  read_rds(
    'data/processed/datos_distribucion_espacial.rds')

datos

# NOTA:
# Cada fila corresponde a una cuadricula de 1 x 1 m.
#
# x, y          = posicion de la cuadricula dentro del area de estudio
# elephantopus  = numero de individuos de Elephantopus mollis
# rhynchospora  = numero de individuos de Rhynchospora nervosa

glimpse(datos)

# Elephantopus mollis -----------------------------------------------------

# Descripcion de la abundancia

resumen_elephantopus <-
  datos %>%
  summarise(
    n = n(),
    media = mean(elephantopus),
    varianza = var(elephantopus),
    indice_dispersion =
      varianza / media)

resumen_elephantopus

# INTERPRETE:
# Compare la media y la varianza.
#
# El indice de dispersion se calcula como:
#
# varianza / media
#
# Interpretacion general:
#
# indice aproximadamente igual a 1 = distribucion aleatoria
# indice mayor que 1               = distribucion agrupada
# indice menor que 1               = distribucion uniforme
#
# IMPORTANTE:
# El indice de dispersion describe la relacion entre la media y la
# varianza, pero por si solo no demuestra estadisticamente que el
# patron difiera de la aleatoriedad.


# Prueba de aleatoriedad

# H0:
# La distribucion de los individuos es compatible con un patron
# espacial aleatorio.
#
# H1:
# La distribucion de los individuos difiere de un patron aleatorio.
#
# Si P < 0.05, se rechaza H0.
#
# IMPORTANTE:
# La prueba determina si el patron difiere de la aleatoriedad.
# La direccion del patron se interpreta utilizando conjuntamente
# el indice de dispersion.

n_elephantopus <-
  nrow(datos)

id_elephantopus <-
  resumen_elephantopus %>%
  pull(indice_dispersion)

chi_elephantopus <-
  (n_elephantopus - 1) *
  id_elephantopus

gl_elephantopus <-
  n_elephantopus - 1

p_elephantopus <-
  pchisq(
    chi_elephantopus,
    df = gl_elephantopus,
    lower.tail = FALSE)

chi_elephantopus
p_elephantopus

# RESULTADO:
# Los individuos de Elephantopus mollis mostraron una distribucion espacial agrupada, con una varianza considerablemente mayor que la media y un indice de dispersion superior a 1
# (χ² = 191.8, gl = 49, P < 0.001).


# Distribucion binomial negativa ------------------------------------------

# Cuando los individuos se encuentran agregados, los conteos suelen
# presentar una varianza mayor que la media.
#
# Una distribucion frecuentemente utilizada para representar este
# tipo de conteos es la binomial negativa.
#
# Primero estimaremos sus dos parametros:
#
# mu   = abundancia media
# size = parametro relacionado con la dispersion de los conteos.

modelo_elephantopus <-
  fitdistr(
    datos$elephantopus,
    densfun = 'negative binomial')

modelo_elephantopus

size_elephantopus <-
  modelo_elephantopus$estimate['size']

mu_elephantopus <-
  modelo_elephantopus$estimate['mu']


# Frecuencias observadas y esperadas

frecuencia_elephantopus <-
  datos %>%
  count(
    elephantopus,
    name = 'observada') %>%
  complete(
    elephantopus = 0:max(elephantopus),
    fill = list(
      observada = 0)) %>%
  mutate(
    esperada =
      dnbinom(
        elephantopus,
        size = size_elephantopus,
        mu = mu_elephantopus) *
      nrow(datos))

frecuencia_elephantopus

# Prueba de bondad de ajuste

# H0:
# Los conteos de Elephantopus mollis son compatibles con una
# distribucion binomial negativa.
#
# H1:
# Los conteos no son compatibles con una distribucion
# binomial negativa.
#
# Para aplicar la prueba chi-cuadrado agrupamos abundancias de manera
# que las frecuencias esperadas sean suficientemente grandes.

observadas_bn <-
  c(
    sum(datos$elephantopus <= 2),
    sum(datos$elephantopus >= 3 & datos$elephantopus <= 5),
    sum(datos$elephantopus >= 6 & datos$elephantopus <= 8),
    sum(datos$elephantopus >= 9 & datos$elephantopus <= 12),
    sum(datos$elephantopus >= 13))

esperadas_bn <-
  c(
    pnbinom(
      2,
      size = size_elephantopus,
      mu = mu_elephantopus),
    pnbinom(
      5,
      size = size_elephantopus,
      mu = mu_elephantopus) -
      pnbinom(
        2,
        size = size_elephantopus,
        mu = mu_elephantopus),
    pnbinom(
      8,
      size = size_elephantopus,
      mu = mu_elephantopus) -
      pnbinom(
        5,
        size = size_elephantopus,
        mu = mu_elephantopus),
    pnbinom(
      12,
      size = size_elephantopus,
      mu = mu_elephantopus) -
      pnbinom(
        8,
        size = size_elephantopus,
        mu = mu_elephantopus),
    pnbinom(
      12,
      size = size_elephantopus,
      mu = mu_elephantopus,
      lower.tail = FALSE)) *
  nrow(datos)

observadas_bn
esperadas_bn

# Se estimaron dos parametros de la distribucion: mu y size.
#
# Los grados de libertad se calculan como:
#
# numero de clases - 1 - numero de parametros estimados

chi_bn <-
  sum(
    (observadas_bn - esperadas_bn)^2 /
      esperadas_bn)

gl_bn <-
  length(observadas_bn) - 1 - 2

p_bn <-
  pchisq(
    chi_bn,
    df = gl_bn,
    lower.tail = FALSE)

chi_bn
gl_bn
p_bn

# RESULTADO:
# Los conteos de Elephantopus mollis fueron compatibles con una
# distribucion binomial negativa (χ² = 0.3, gl = 2, P = 0.871).
# Este resultado concuerda con la sobredispersion observada y con
# el patron espacial agrupado identificado previamente.


# Rhynchospora nervosa ----------------------------------------------------

# Descripcion de la abundancia

resumen_rhynchospora <-
  datos %>%
  summarise(
    n = n(),
    media = mean(rhynchospora),
    varianza = var(rhynchospora),
    indice_dispersion =
      varianza / media)

resumen_rhynchospora

# INTERPRETE:
# En una poblacion distribuida aleatoriamente, la media y la
# varianza de los conteos tienden a ser similares.
#
# Examine el indice de dispersion.
# ¿Se encuentra cercano a 1?


# Prueba de aleatoriedad

# Utilizamos las mismas hipotesis evaluadas para Elephantopus mollis:
#
# H0:
# La distribucion de los individuos es compatible con un patron
# espacial aleatorio.
#
# H1:
# La distribucion de los individuos difiere de un patron aleatorio.

n_rhynchospora <-
  nrow(datos)

id_rhynchospora <-
  resumen_rhynchospora %>%
  pull(indice_dispersion)

chi_rhynchospora <-
  (n_rhynchospora - 1) *
  id_rhynchospora

gl_rhynchospora <-
  n_rhynchospora - 1

p_rhynchospora <-
  pchisq(
    chi_rhynchospora,
    df = gl_rhynchospora,
    lower.tail = FALSE)

chi_rhynchospora
p_rhynchospora

# RESULTADO:
# La variacion en la abundancia de Rhynchospora nervosa entre
# cuadriculas no difirio de la esperada bajo un patron espacial
# aleatorio (χ² = 48.0, gl = 49, P = 0.512). El indice de dispersion
# cercano a 1 tambien es consistente con este patron.


# Distribucion de Poisson -------------------------------------------------

# Una poblacion distribuida aleatoriamente puede producir conteos
# compatibles con una distribucion de Poisson.
#
# En una distribucion de Poisson:
#
# media aproximadamente igual a varianza
#
# El unico parametro de la distribucion, lambda, corresponde a la
# abundancia media esperada.

lambda_rhynchospora <-
  mean(datos$rhynchospora)

# Frecuencias observadas y esperadas

frecuencia_rhynchospora <-
  datos %>%
  count(
    rhynchospora,
    name = 'observada') %>%
  complete(
    rhynchospora = 0:max(rhynchospora),
    fill = list(
      observada = 0)) %>%
  mutate(
    esperada =
      dpois(
        rhynchospora,
        lambda = lambda_rhynchospora) *
      nrow(datos))

frecuencia_rhynchospora

# Prueba de bondad de ajuste

# H0:
# Los conteos de Rhynchospora nervosa son compatibles con una
# distribucion de Poisson.
#
# H1:
# Los conteos no son compatibles con una distribucion de Poisson.
#
# Las abundancias mayores o iguales a tres se agrupan para mantener
# frecuencias esperadas adecuadas para la prueba chi-cuadrado.

observadas_poisson <-
  c(
    sum(datos$rhynchospora == 0),
    sum(datos$rhynchospora == 1),
    sum(datos$rhynchospora == 2),
    sum(datos$rhynchospora >= 3))

esperadas_poisson <-
  c(
    dpois(
      0,
      lambda_rhynchospora),
    dpois(
      1,
      lambda_rhynchospora),
    dpois(
      2,
      lambda_rhynchospora),
    ppois(
      2,
      lambda_rhynchospora,
      lower.tail = FALSE)) *
  nrow(datos)

observadas_poisson
esperadas_poisson

# Se estimo un parametro de la distribucion: lambda.
#
# Los grados de libertad se calculan como:
#
# numero de clases - 1 - numero de parametros estimados

chi_poisson <-
  sum(
    (observadas_poisson - esperadas_poisson)^2 /
      esperadas_poisson)

gl_poisson <-
  length(observadas_poisson) - 1 - 1

p_poisson <-
  pchisq(
    chi_poisson,
    df = gl_poisson,
    lower.tail = FALSE)

chi_poisson
gl_poisson
p_poisson

# RESULTADO:
# Los conteos registrados para Rhynchospora nervosa mostraron un
# ajuste adecuado a la distribucion de Poisson (χ² = 2.8, gl = 2,
# P = 0.249), proporcionando evidencia adicional consistente con
# una distribucion espacial aleatoria.


# Cuadro 1 ---------------------------------------------------------------

# Resumen de los principales resultados para ambas especies.

cuadro_resultados <-
  tibble(
    especie = c(
      'Elephantopus mollis',
      'Rhynchospora nervosa'),
    media = c(
      mean(datos$elephantopus),
      mean(datos$rhynchospora)),
    varianza = c(
      var(datos$elephantopus),
      var(datos$rhynchospora)),
    indice_dispersion = c(
      id_elephantopus,
      id_rhynchospora),
    patron = c(
      'Agrupado',
      'Aleatorio'),
    distribucion = c(
      'Binomial negativa',
      'Poisson'))

cuadro_resultados

# Formato del cuadro

cuadro_1 <-
  cuadro_resultados %>%
  gt() %>%
  cols_label(
    especie = 'Especie',
    media = 'Media',
    varianza = 'Varianza',
    indice_dispersion = 'ID',
    patron = 'Patrón espacial',
    distribucion = 'Distribución teórica') %>%
  fmt_number(
    columns = c(
      media,
      varianza,
      indice_dispersion),
    decimals = 2) %>%
  text_transform(
    locations = cells_body(
      columns = especie),
    fn = function(x) {
      paste0(
        '<i>',
        x,
        '</i>')
    }) %>%
  tab_source_note(
    source_note = md(
      '*Nota:* ID = índice de dispersión (varianza/media).'))

cuadro_1

# Guardar cuadro ----------------------------------------------------------

dir.create(
  'output/tables',
  recursive = TRUE,
  showWarnings = FALSE)

gtsave(
  cuadro_1,
  'output/tables/cuadro_1_distribucion_espacial.docx')

# Figura 1: distribucion espacial -----------------------------------------

# El tamaño de los simbolos representa la abundancia registrada
# en cada cuadricula de 1 m².
#
# Debido a las diferencias de abundancia entre las especies,
# cada panel utiliza su propia escala de tamaño.


# Elephantopus mollis

figura_1a <-
  datos %>%
  ggplot(
    aes(
      x = x,
      y = y)) +
  geom_point(
    aes(
      size = elephantopus),
    shape = 21,
    fill = '#D55E00',
    color = 'black',
    stroke = 0.3) +
  scale_size_continuous(
    name = 'Individuos/m²',
    breaks = c(
      0,
      5,
      10,
      15,
      20),
    range = c(
      0.5,
      6)) +
  scale_x_continuous(
    limits = c(
      0,
      100),
    breaks = seq(
      0,
      100,
      20),
    expand = c(
      0,
      0)) +
  scale_y_continuous(
    limits = c(
      0,
      100),
    breaks = seq(
      0,
      100,
      20),
    expand = c(
      0,
      0)) +
  coord_fixed() +
  labs(
    x = 'X (m)',
    y = 'Y (m)') +
  theme_classic() +
  theme(
    legend.position = 'bottom',
    legend.title = element_text(
      size = 9),
    legend.text = element_text(
      size = 8),
    legend.key.width = unit(
      0.6,
      'cm'))

figura_1a

# Rhynchospora nervosa

figura_1b <-
  datos %>%
  ggplot(
    aes(
      x = x,
      y = y)) +
  geom_point(
    aes(
      size = rhynchospora),
    shape = 21,
    fill = '#0072B2',
    color = 'black',
    stroke = 0.3) +
  scale_size_continuous(
    name = 'Individuos/m²',
    breaks = c(
      0,
      2,
      4,
      5),
    range = c(
      0.5,
      6)) +
  scale_x_continuous(
    limits = c(
      0,
      100),
    breaks = seq(
      0,
      100,
      20),
    expand = c(
      0,
      0)) +
  scale_y_continuous(
    limits = c(
      0,
      100),
    breaks = seq(
      0,
      100,
      20),
    expand = c(
      0,
      0)) +
  coord_fixed() +
  labs(
    x = 'X (m)',
    y = 'Y (m)') +
  theme_classic() +
  theme(
    legend.position = 'bottom',
    legend.title = element_text(
      size = 9),
    legend.text = element_text(
      size = 8),
    legend.key.width = unit(
      0.6,
      'cm'))

figura_1b

# Combinar paneles

figura_1 <-
  plot_grid(
    figura_1a,
    figura_1b,
    labels = c(
      'A',
      'B'),
    label_size = 12,
    label_fontface = 'bold',
    nrow = 1,
    align = 'hv',
    axis = 'tblr')

figura_1

# Guardar Figura 1

ggsave(
  'output/figures/figura_1_distribucion_espacial.jpg',
  figura_1,
  width = 9,
  height = 5,
  units = 'in',
  dpi = 300)

# Figura 2: distribuciones teoricas ---------------------------------------

# Las barras representan las frecuencias observadas.
# Los puntos y las lineas representan las frecuencias esperadas segun
# la distribucion teorica evaluada para cada especie.


# Elephantopus mollis: binomial negativa

figura_2a <-
  frecuencia_elephantopus %>%
  ggplot(
    aes(
      x = elephantopus)) +
  geom_col(
    aes(
      y = observada),
    fill = '#D55E00',
    color = 'black',
    alpha = 0.8) +
  geom_line(
    aes(
      y = esperada),
    linewidth = 0.8) +
  geom_point(
    aes(
      y = esperada),
    size = 2.5) +
  scale_x_continuous(
    breaks = seq(
      0,
      max(frecuencia_elephantopus$elephantopus),
      2)) +
  labs(
    x = 'Individuos/m²',
    y = 'Número de cuadrículas') +
  theme_classic() +
  theme(
    plot.title = element_text(
      face = 'italic'))

figura_2a

# Rhynchospora nervosa: Poisson

figura_2b <-
  frecuencia_rhynchospora %>%
  ggplot(
    aes(
      x = rhynchospora)) +
  geom_col(
    aes(
      y = observada),
    fill = '#0072B2',
    color = 'black',
    alpha = 0.8) +
  geom_line(
    aes(
      y = esperada),
    linewidth = 0.8) +
  geom_point(
    aes(
      y = esperada),
    size = 2.5) +
  scale_x_continuous(
    breaks =
      0:max(frecuencia_rhynchospora$rhynchospora)) +
  labs(
    x = 'Individuos/m²',
    y = NULL) +
  theme_classic() +
  theme(
    plot.title = element_text(
      face = 'italic'))

figura_2b

# Combinar paneles

figura_2 <-
  plot_grid(
    figura_2a,
    figura_2b,
    labels = c(
      'A',
      'B'),
    nrow = 1,
    align = 'hv')

figura_2


# Guardar Figura 2

ggsave(
  'output/figures/figura_2_distribuciones_teoricas.jpg',
  figura_2,
  width = 10,
  height = 5,
  units = 'in',
  dpi = 300)
