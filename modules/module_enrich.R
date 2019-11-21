setup_enrich_ui <- function(id) {
    ns <- NS(id)
    tabPanel(
        id,
        fluidPage(
            fluidRow(
                column(8,
                       h4("Some ideas for development"),
                       htmlOutput(ns("html"))
                )
            )
        )
    )
}

module_enrich_server <- function(input, output, session) {
    
    output$html <- renderUI({
        
        entries <- c(
            "Could enrich for the selected subsets compared to global data and illustrate with entire clusterProfiler package, but is would require IDs matching some particular GO databases. Should maybe skip this one."
        )
        
        html_string <- paste0(
            "<li>",
            paste(entries, collapse="</li><li>"),
            "</li>"
        )
        
        HTML(sprintf("<ul>%s</ul>", html_string))
    })
}

