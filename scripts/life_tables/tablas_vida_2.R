# Ecologia de poblaciones
# Profesor Hector Zumbado Ulate
# Tablas de vida II

# setup -------------------------------------------------------------------

rm(list = ls())

library(cowplot)
library(grid)
library(tidyverse)

# data --------------------------------------------------------------------

datos <-
  read_csv(
    'data/processed/datos_cementerios.csv') %>%
  mutate(
    edad = factor(
      edad,
      levels = c(
        '0-01', '02-05', '06-10', '11-15',
        '16-20', '21-25', '26-30', '31-35',
        '36-40', '41-45', '46-50', '51-55',
        '56-60', '61-65', '66-70', '71-75',
        '76-80', '81-85', '86-90', '91-95',
        '96+')))

life_table <-
  read_csv(
    'data/processed/life_table.csv') %>%
  mutate(
    edad = factor(
      edad,
      levels = c(
        '0-01', '02-05', '06-10', '11-15',
        '16-20', '21-25', '26-30', '31-35',
        '36-40', '41-45', '46-50', '51-55',
        '56-60', '61-65', '66-70', '71-75',
        '76-80', '81-85', '86-90', '91-95',
        '96+')))

# colores -----------------------------------------------------------------

colores <-
  c(
    'Heredia' = 'cornflowerblue',
    'San José' = '#9C7A00',
    'Femenino' = 'cornflowerblue',
    'Masculino' = '#9C7A00')

# curvas teoricas ---------------------------------------------------------

supervivencia_final <-
  0.01

h <-
  -log(
    supervivencia_final)

curvas_teoricas <-
  tibble(
    edad_relativa = seq(
      0,
      1,
      length.out = 101),
    tipo_I = exp(
      -h * edad_relativa^5),
    tipo_II = exp(
      -h * edad_relativa),
    tipo_III = exp(
      -h * edad_relativa^0.2)) %>%
  pivot_longer(
    cols = tipo_I:tipo_III,
    names_to = 'tipo',
    values_to = 'lx') %>%
  mutate(
    tipo = recode(
      tipo,
      tipo_I = 'Tipo I',
      tipo_II = 'Tipo II',
      tipo_III = 'Tipo III'))

# grafico de curvas teoricas ----------------------------------------------
p1 <-
  curvas_teoricas %>%
  ggplot(aes(
    x = edad_relativa,
    y = lx,
    color = tipo,
    linetype = tipo,
    shape = tipo)) +
  geom_line(
    linewidth = 1.2) +
  geom_point(
    data = curvas_teoricas %>%
      group_by(
        tipo) %>%
      slice(
        seq(
          1,
          n(),
          by = 10)) %>%
      ungroup(),
    size = 2.2) +
  scale_y_log10(
    breaks = c(
      1,
      0.1,
      0.01),
    labels = c(
      '1',
      '0.1',
      '0.01')) +
  scale_color_manual(
    values = c(
      'Tipo I' = '#17365D',
      'Tipo II' = '#9C7A00',
      'Tipo III' = '#6B705C')) +
  scale_linetype_manual(
    values = c(
      'Tipo I' = 'solid',
      'Tipo II' = 'dashed',
      'Tipo III' = 'dotted')) +
  scale_shape_manual(
    values = c(
      'Tipo I' = 16,
      'Tipo II' = 15,
      'Tipo III' = 17)) +
  labs(
    x = 'Edad relativa',
    y = expression(l[x]),
    color = 'Curva',
    linetype = 'Curva',
    shape = 'Curva') +
  theme_classic() +
  theme(
    panel.grid.major.y = element_line(
      linewidth = 0.3,
      color = 'grey85'),
    axis.text = element_text(
      size = 11),
    axis.title = element_text(
      size = 12),
    legend.position = 'right',
    legend.title = element_text(
      face = 'bold'),
    legend.key.width = unit(
      1.3,
      'cm'))

p1

# supervivencia observada por sitio --------------------------------------

p2 <-
  life_table %>%
  ggplot(aes(
    x = edad,
    y = lxt,
    group = sitio,
    color = sitio,
    linetype = sitio)) +
  geom_line(
    linewidth = 1) +
  geom_point(
    size = 1.8) +
  scale_color_manual(
    values = colores[c(
      'Heredia',
      'San José')]) +
  labs(
    x = 'Edad',
    y = expression(l[x]),
    color = 'Sitio',
    linetype = 'Sitio') +
  theme_classic() +
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12),
    legend.position = 'right')

p2

# supervivencia observada por sexo ---------------------------------------

supervivencia_sexo <-
  life_table %>%
  select(
    sitio,
    edad,
    lxf,
    lxm) %>%
  pivot_longer(
    cols = c(
      lxf,
      lxm),
    names_to = 'sexo',
    values_to = 'lx') %>%
  mutate(
    sexo = recode(
      sexo,
      lxf = 'Femenino',
      lxm = 'Masculino'))

p2

p3 <-
  supervivencia_sexo %>%
  mutate(
    intervalo = as.numeric(
      edad)) %>%
  ggplot(aes(
    x = intervalo,
    y = lx,
    group = sexo,
    color = sexo)) +
  geom_line(
    linewidth = 1.2) +
  geom_point(
    size = 1.8) +
  facet_wrap(
    ~ sitio,
    nrow = 1) +
  scale_color_manual(
    values = c(
      'Femenino' = '#17365D',
      'Masculino' = '#9C7A00')) +
  scale_x_continuous(
    breaks = c(
      1,
      5,
      9,
      13,
      17,
      21),
    labels = c(
      '0-01',
      '16-20',
      '36-40',
      '56-60',
      '76-80',
      '96+'),
    expand = expansion(
      mult = c(
        0.03,
        0.03))) +
  scale_y_continuous(
    limits = c(
      0,
      1),
    breaks = seq(
      0,
      1,
      by = 0.25),
    expand = expansion(
      mult = c(
        0,
        0.02))) +
  labs(
    x = 'Edad',
    y = expression(l[x]),
    color = 'Sexo') +
  theme_classic() +
  theme(
    axis.text.x = element_text(
      size = 9),
    axis.text.y = element_text(
      size = 10),
    axis.title = element_text(
      size = 12),
    strip.text = element_text(
      size = 11,
      face = 'bold'),
    panel.spacing.x = grid::unit(
      1,
      'cm'),
    legend.position = 'bottom',
    legend.title = element_text(
      face = 'bold'))

p3

# distribucion de edad de muerte -----------------------------------------

p4 <-
  datos %>%
  mutate(
    sexo = recode(
      sexo,
      femenino = 'Femenino',
      masculino = 'Masculino')) %>%
  ggplot(aes(
    x = sexo,
    y = edad_muerte,
    fill = sexo)) +
  geom_boxplot(
    width = 0.6,
    alpha = 0.85,
    outlier.alpha = 0.3,
    color = 'black',
    linewidth = 0.3) +
  scale_fill_manual(
    values = colores[c(
      'Femenino',
      'Masculino')]) +
  labs(
    x = NULL,
    y = 'Edad de muerte') +
  theme_classic() +
  theme(
    axis.text = element_text(size = 11),
    axis.title = element_text(size = 12),
    legend.position = 'none')

p4

# piramide de cohorte reconstruida ---------------------------------------

piramide_cohorte <-
  life_table %>%
  select(
    sitio,
    edad,
    nxf,
    nxm) %>%
  pivot_longer(
    cols = c(
      nxf,
      nxm),
    names_to = 'sexo',
    values_to = 'nx') %>%
  mutate(
    sexo = recode(
      sexo,
      nxf = 'Femenino',
      nxm = 'Masculino'),
    nx_plot = if_else(
      sexo == 'Femenino',
      -nx,
      nx))

p5 <-
  piramide_cohorte %>%
  ggplot(aes(
    x = edad,
    y = nx_plot,
    fill = sexo)) +
  geom_col(
    color = 'black',
    width = 0.9) +
  coord_flip() +
  facet_wrap(
    ~ sitio,
    nrow = 1) +
  scale_y_continuous(
    labels = abs) +
  scale_fill_manual(
    values = colores[c(
      'Femenino',
      'Masculino')]) +
  labs(
    x = 'Edad',
    y = expression(n[x]),
    fill = 'Sexo') +
  theme_classic() +
  theme(
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12),
    strip.text = element_text(size = 11),
    legend.position = 'right')

p5

# piramide de mortalidad --------------------------------------------------

piramide_mortalidad <-
  datos %>%
  count(
    sitio,
    edad,
    sexo,
    .drop = FALSE) %>%
  mutate(
    sexo = recode(
      sexo,
      femenino = 'Femenino',
      masculino = 'Masculino'),
    n_plot = if_else(
      sexo == 'Femenino',
      -n,
      n))

piramide_mortalidad

p6 <-
  piramide_mortalidad %>%
  ggplot(aes(
    x = edad,
    y = n_plot,
    fill = sexo)) +
  geom_col(
    color = 'black',
    width = 0.9) +
  coord_flip() +
  facet_wrap(
    ~ sitio,
    nrow = 1) +
  scale_y_continuous(
    labels = abs) +
  scale_fill_manual(
    values = colores[c(
      'Femenino',
      'Masculino')]) +
  labs(
    x = 'Edad de muerte',
    y = 'Frecuencia',
    fill = 'Sexo') +
  theme_classic() +
  theme(
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12),
    strip.text = element_text(size = 11),
    legend.position = 'right')

p6

# resumen estadistico por sitio ------------------------------------------

resumen_sitio <-
  datos %>%
  summarize(
    n = n(),
    mediana = median(
      edad_muerte),
    q1 = quantile(
      edad_muerte,
      0.25),
    q3 = quantile(
      edad_muerte,
      0.75),
    .by = sitio)

resumen_sitio

# resumen estadistico por sexo -------------------------------------------

resumen_sexo <-
  datos %>%
  summarize(
    n = n(),
    mediana = median(
      edad_muerte),
    q1 = quantile(
      edad_muerte,
      0.25),
    q3 = quantile(
      edad_muerte,
      0.75),
    .by = sexo)

resumen_sexo

# comparacion entre sitios ------------------------------------------------

edad_heredia <-
  datos %>%
  filter(
    sitio == 'Heredia') %>%
  pull(
    edad_muerte)

edad_san_jose <-
  datos %>%
  filter(
    sitio == 'San José') %>%
  pull(
    edad_muerte)

ks_sitio <-
  ks.test(
    edad_heredia,
    edad_san_jose,
    exact = FALSE)

mw_sitio <-
  wilcox.test(
    edad_heredia,
    edad_san_jose,
    exact = FALSE)

ks_sitio

# Las distribuciones de edad de muerte no difirieron entre Heredia (n = 1403) y San José (n = 1360; KS: D = 0.08, p = 0.24).

mw_sitio

# no hay evidencia de que las edades de muerte tiendan a ser mayores o menores en una localidad que en la otra (Mann–Whitney: W = 965357, p = 0.589).

# comparacion entre sexos -------------------------------------------------

edad_femenino <-
  datos %>%
  filter(
    sexo == 'femenino') %>%
  pull(
    edad_muerte)

edad_masculino <-
  datos %>%
  filter(
    sexo == 'masculino') %>%
  pull(
    edad_muerte)

ks_sexo <-
  ks.test(
    edad_femenino,
    edad_masculino,
    exact = FALSE)

mw_sexo <-
  wilcox.test(
    edad_femenino,
    edad_masculino,
    exact = FALSE)

ks_sexo

# Las mujeres mostraron una mayor supervivencia hasta edades avanzadas que los hombres (KS: D = 0.178, p < 0.001).

mw_sexo

# Las mujeres tendieron a morir a edades más avanzadas que los hombres (Mann–Whitney: W = 1167587, p < 0.001).

# figura 1 ---------------------------------------------------------------

figura_1 <-
  cowplot::plot_grid(
    p1,
    p2,
    labels = c(
      'A)',
      'B)'),
    nrow = 1)

figura_1

ggsave(
  'output/figures/figura_1_supervivencia.jpg',
  plot = figura_1,
  width = 12,
  height = 5.5,
  dpi = 300)

# figura 2 ---------------------------------------------------------------

figura_2 <-
  cowplot::plot_grid(
    p3,
    p4,
    labels = c(
      'A)',
      'B)'),
    nrow = 1,
    rel_widths = c(
      1.4,
      0.8))

figura_2

ggsave(
  'output/figures/figura_2_comparaciones.jpg',
  plot = figura_2,
  width = 12,
  height = 5.5,
  dpi = 300)

# figura 3 ---------------------------------------------------------------

figura_3 <-
  cowplot::plot_grid(
    p5,
    p6,
    labels = c(
      'A)',
      'B)'),
    nrow = 1)

figura_3

ggsave(
  'output/figures/figura_3_piramides.jpg',
  plot = figura_3,
  width = 14,
  height = 6,
  dpi = 300)
