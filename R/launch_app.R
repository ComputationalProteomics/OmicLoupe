#' launches the shinyAppDemo app
#'
#' @export runApp
#'
#' @return shiny application object
#'
#' @importFrom ggdendro theme_dendro segment label
#' @import dplyr
#' @importFrom forcats fct_collapse
#' @import ggplot2
#' @importFrom ggpubr ggarrange
#' @importFrom jsonlite toJSON
#' @importFrom plotly ggplotly plot_ly renderPlotly toWebGL plotlyOutput config event_data
#' @importFrom purrr walk map discard keep
#' @importFrom R6 R6Class
#' @importFrom readr read_tsv cols write_tsv
#' @importFrom GGally ggpairs
#' @importFrom rlang UQ
#' @import shiny
#' @importFrom shinyalert shinyalert
#' @importFrom shinycssloaders withSpinner
#' @importFrom stats complete.cases cor.test prcomp quantile reorder setNames var
#' @importFrom stringr str_length str_split str_length str_trunc
#' @importFrom tidyr pivot_longer gather unite
#' @importFrom utils head packageVersion
runApp <- function() {
    
    options(shiny.maxRequestSize=100*1024^2)
    ggplot2::theme_set(ggplot2::theme_classic())
    
    message(sprintf("Running OmicLoupe version %s", packageVersion("OmicLoupe")))
    
    shinyApp(ui = get_ui(), server = get_server())
}


if(getRversion() >= "2.15.1")  utils::globalVariables(c("."))

