setup_correlation_ui <- function(id) {
    ns <- NS(id)
    tabPanel(
        id,
        fluidPage(
            fluidRow(
                column(8,
                       h4("Some ideas for development"),
                       htmlOutput(ns("html"))
                ),
                htmlOutput(ns("warnings")),
                tabsetPanel(
                    type = "tabs",
                    tabPanel("Combined view", plotOutput(ns("spot_display_combined")))
                ),
                tabsetPanel(
                    type = "tabs",
                    tabPanel("Combined data", DT::DTOutput(ns("table_display_combined")))
                )
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

module_correlation_server <- function(input, output, session, rv) {
    
    output$html <- renderUI({
        HTML(parse_vector_to_bullets(c(
            "Show correlation histograms",
            "Other correlation visualizations?"
        )))
    })
}










