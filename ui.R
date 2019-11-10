library(shiny)
library(R6)
library(plotly)

print("UI")

source("modules/module_setup_ui.R")
source("modules/module_vis_ui.R")
source("modules/module_plotly_ui.R")
source("R/MapObject.R")
source("R/shared_visualization_setup.R")

ui <- function() {
    shinyUI({
        navbarPage(
            theme = shinythemes::shinytheme("flatly"),
            "OmicLoupe",
            id="navbar",
            setup_panel_ui("Setup"),
            setup_visual_ui("Visual"),
            setup_plotly_ui("Plotly")
        )
    })
}
