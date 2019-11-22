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



module_enrich_server <- function(input, output, session) {
    
    output$html <- renderUI({
        
        HTML(parse_vector_to_bullets(c(
            "Could enrich for the selected subsets compared to global data and illustrate with entire clusterProfiler package, but is would require IDs matching some particular GO databases. Should maybe skip this one."
        )))
    })
}

