setup_plotly_ui <- function(id) {
    
    ns <- NS(id)
    tabPanel(
        id,
        fluidPage(
            fluidRow(
                column(4,
                       data_display_ui_panel(ns),
                       minimaps_panel(ns)
                ),
                column(8,
                       column(6,
                              plotlyOutput(ns("plotly_volc1")),
                              plotlyOutput(ns("plotly_ma1")),
                              plotlyOutput(ns("plotly_hist1"))
                       ),
                       column(6,
                              plotlyOutput(ns("plotly_volc2")),
                              plotlyOutput(ns("plotly_ma2")),
                              plotlyOutput(ns("plotly_hist2"))
                       )
                )
            )
        )
    )
}
