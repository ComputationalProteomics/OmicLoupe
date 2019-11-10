library(shiny)
library(tidyverse)

options(shiny.maxRequestSize=100*1024^2)

print("Outside")

source("modules/module_setup_server.R")
source("modules/module_vis_server.R")
source("modules/module_plotly_server.R")

server <- shinyServer(function(session, input, output) {
    
    reactive_values <- callModule(module_setup_server, id="Setup")
    callModule(module_visual_server, id="Visual", reactive_values)
    callModule(module_plotly_server, id="Plotly", reactive_values)
})


