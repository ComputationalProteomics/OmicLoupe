setup_help_ui <- function(id) {
    ns <- NS(id)
    tabPanel(
        id,
        fluidPage(
            fluidRow(
                column(8,
                       p("help!")
                )
            )
        )
    )
}

module_help_server <- function(input, output, session) {

}