setup_quality_ui <- function(id) {
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

module_quality_server <- function(input, output, session) {
    
    output$html <- renderUI({
        
        entries <- c(
            "Density plots (colored from design)",
            "Box plots (colored from design)",
            "Total intensity / total missing (colored from design)",
            "Tree plot",
            "Histograms (intensity, retentiontime, whichever column)",
            "Design histograms (how target characteristic is distributed)"
        )
        
        html_string <- paste0(
            "<li>",
            paste(entries, collapse="</li><li>"),
            "</li>"
        )
        
        HTML(sprintf("<ul>%s</ul>", html_string))
    })
}

