quiet <- suppressPackageStartupMessages

quiet(library(DT))
quiet(library(GGally))
quiet(library(ggdendro))
quiet(library(ggrepel))
quiet(library(plotly))
quiet(library(R6))
quiet(library(rlang))
quiet(library(shiny))
quiet(library(shinyjs))
quiet(library(shinyalert))
quiet(library(shinycssloaders))
quiet(library(stringr))
quiet(library(stringi))
quiet(library(tidyverse))
quiet(library(jsonlite))

quiet(library(conflicted))

devtools::load_all()
runApp()
