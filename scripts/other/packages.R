# Hector Zumbado
# packages needed to run the project

# setup -------------------------------------------------------------------

rm(list = ls())

ipak <-
  function(pkg){
    new.pkg <-
      pkg[!(pkg %in% installed.packages()[, "Package"])]
    if (length(new.pkg))
      install.packages(new.pkg, dependencies = TRUE)
    sapply(pkg, require, character.only = TRUE)}

packages <-
  c(
    'janitor',
    'rinat',
    'tidyverse',
    'rgbif',
    'CoordinateCleaner',
    'tmap',
    'sf',
    'terra',
    'remotes',
    'devtools',
    'usethis',
    'cols4all',
    'vegan',
    'ecospat')

ipak(packages)

#other packages

remotes::install_github("ropensci/scrubr")
devtools::install_github("jasonleebrown/humboldt")
