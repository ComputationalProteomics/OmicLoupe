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
            ),
            tabsetPanel(
                type = "tabs",
                tabPanel("Boxplots", p("boxplots")),
                tabPanel("Density", p("density")),
                tabPanel("Histograms", p("histograms"))
            )
        )
    )
}


parse_vector_to_bullets <- function(vect, number=TRUE) {
    html_string <- paste0(
        "<li>",
        paste(vect, collapse="</li><li>"),
        "</li>"
    )
    
    if (!number) {
        list_style <- "ul"
    }
    else {
        list_style <- "ol"
    }
    
    sprintf("<%s>%s</%s>", list_style, html_string, list_style)
}

module_quality_server <- function(input, output, session) {
    
    output$html <- renderUI({
        
        HTML(parse_vector_to_bullets(c(
            "Density plots (colored from design)",
            "Box plots (colored from design)",
            "Total intensity / total missing (colored from design)",
            "Tree plot",
            "Histograms (intensity, retentiontime, whichever column)",
            "Design histograms (how target characteristic is distributed)"
        )))
    })
}

