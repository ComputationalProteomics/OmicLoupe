library(shiny)
library(shinyjs)
library(shinyalert)
library(R6)
library(plotly)
library(tidyverse)
library(DT)
library(stringr)

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
source("R/pca.R")
source("R/reactive_values.R")
source("R/MapObject.R")
source("R/shared_visualization_setup.R")

ui <- navbarPage(
    
    theme = shinythemes::shinytheme("flatly"),
    
    "OmicLoupe",
    id="navbar",
    setup_panel_ui("Setup"),
    setup_plotly_ui("Plotly"),
    setup_pca_ui("PCA"),
    setup_quality_ui("Quality"),
    setup_overlap_ui("Overlap"),
    setup_spotcheck_ui("Spotcheck"),
    setup_correlation_ui("Correlation"),
    setup_help_ui("Help"),
    setup_ideas_ui("Ideas")
)

server <- shinyServer(function(session, input, output) {
    
    reactive_values <- callModule(module_setup_server, id="Setup", module_name="Setup")
    callModule(module_plotly_server, id="Plotly", rv=reactive_values, module_name="Plotly")
    callModule(module_pca_server, id="PCA", rv=reactive_values, module_name="PCA")
    callModule(module_help_server, id="Help", module_name="Help")
    callModule(module_quality_server, id="Quality", rv=reactive_values, module_name="Quality")
    callModule(module_overlap_server, id="Overlap", rv=reactive_values, module_name="Overlap")
    callModule(module_spotcheck_server, id="Spotcheck", rv=reactive_values, module_name="Spotcheck")
    callModule(module_correlation_server, id="Correlation", rv=reactive_values, module_name="Correlation")
    callModule(module_ideas_server, id="Ideas", module_name="Ideas")
})

shinyApp(ui = ui, server = server)
