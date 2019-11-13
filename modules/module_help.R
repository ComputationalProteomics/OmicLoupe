setup_help_ui <- function(id) {
    ns <- NS(id)
    tabPanel(
        id,
        fluidPage(
            fluidRow(
                column(8,
                       p("help!"),
                       h4("Help: Setup"),
                       textOutput(ns("help_setup")),
                       h4("Help: Plotly"),
                       textOutput(ns("help_plotly")),
                       h4("Help: PCA"),
                       textOutput(ns("help_pca"))
                )
            )
        )
    )
}

module_help_server <- function(input, output, session) {

    output$help_setup <- renderText({
        "Setup text"
    })
    
    output$help_plotly <- renderText({
        "Plotly text"
    })
    
    output$help_pca <- renderText({
        "PCA text"
    })
    
}