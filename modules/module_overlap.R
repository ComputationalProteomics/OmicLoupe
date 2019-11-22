setup_overlap_ui <- function(id) {
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
            p("Left and right: Venns or upsets")
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



module_overlap_server <- function(input, output, session) {
    
    output$html <- renderUI({

        HTML(parse_vector_to_bullets(c(
            "Illustration of presence of certain features across the two datasets",
            "Illustration of presence of certain features in certain statistical comparisons",
            "Illustration of overlap across features passing certain statistical thresholds",
            "Venn diagrams and upsets are alternatives"
        )))
    })
}

