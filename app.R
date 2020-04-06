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
quiet(library(stringr))
quiet(library(tidyverse))

options(shiny.maxRequestSize=100*1024^2)

source("modules/module_setup.R")
source("modules/module_plotly.R")
source("modules/module_pca.R")
source("modules/module_help.R")
source("modules/module_ideas.R")

source("modules/module_quality.R")
source("modules/module_overlap.R")
source("modules/module_spotcheck.R")
source("modules/module_correlation.R")

source("R/Venn.R")
source("R/quality_plots.R")
source("R/pca.R")
source("R/reactive_values.R")
source("R/MapObject.R")
source("R/shared_visualization_setup.R")
source("R/help_texts.R")

ui <- navbarPage(
    
    theme = shinythemes::shinytheme("flatly"),
    
    title="OmicLoupe",
    id="navbar",
    setup_panel_ui("Setup"),
    
    setup_quality_ui("Quality"),
    setup_pca_ui("PCA"),

    setup_plotly_ui("StatDist"),
    setup_overlap_ui("Overlap"),

    setup_correlation_ui("Correlation"),
    setup_spotcheck_ui("Spotcheck"),

    setup_help_ui("Help")
)

server <- shinyServer(function(session, input, output) {
  
    reactive_values <- callModule(module_setup_server, id="Setup", module_name="Setup")
    callModule(module_quality_server, id="Quality", rv=reactive_values, module_name="Quality")
    callModule(module_pca_server, id="PCA", rv=reactive_values, module_name="PCA")
    callModule(module_spotcheck_server, id="Spotcheck", rv=reactive_values, module_name="Spotcheck")

    callModule(module_plotly_server, id="StatDist", rv=reactive_values, module_name="StatDist")
    callModule(module_overlap_server, id="Overlap", rv=reactive_values, module_name="Overlap")

    callModule(module_correlation_server, id="Correlation", rv=reactive_values, module_name="Correlation")

    callModule(module_help_server, id="Help", module_name="Help")
    callModule(module_ideas_server, id="Ideas", module_name="Ideas")
})

shinyApp(ui = ui, server = server)
