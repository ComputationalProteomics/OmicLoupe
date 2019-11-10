setup_visual_ui <- function(id) {
    
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
                       verbatimTextOutput(ns("correlation_vals")),
                       plotOutput(ns("pvalhists")),
                       plotOutput(ns("maplots")),
                       plotOutput(ns("volcanos")),
                       plotOutput(ns("custom_comparison")),
                       plotOutput(ns("exact_fold_comparison"))
                )
            )
        )
    )
}


