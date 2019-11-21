setup_spotcheck_ui <- function(id) {
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

module_spotcheck_server <- function(input, output, session) {
    
    output$html <- renderUI({
        
        entries <- c(
            "Allow rapidly switch here after identifying features in other tab",
            "Intensity illustration across characteristic from design (boxplot / scatter)",
            "Profile illustrations allowing display of multiple features (sorted on design condition)",
            "Possibly sequence alignment - But would require rework in the backend as we then need to include sequence data. I recommend against."
        )
        
        html_string <- paste0(
            "<li>",
            paste(entries, collapse="</li><li>"),
            "</li>"
        )
        
        HTML(sprintf("<ul>%s</ul>", html_string))
    })
}

