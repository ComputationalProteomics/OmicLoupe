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
            )
        )
    )
}

module_overlap_server <- function(input, output, session) {
    
    output$html <- renderUI({
        
        entries <- c(
            "Illustration of presence of certain features across the two datasets",
            "Illustration of presence of certain features in certain statistical comparisons",
            "Illustration of overlap across features passing certain statistical thresholds",
            "Venn diagrams and upsets are alternatives"
        )
        
        html_string <- paste0(
            "<li>",
            paste(entries, collapse="</li><li>"),
            "</li>"
        )
        
        HTML(sprintf("<ul>%s</ul>", html_string))
    })
}

