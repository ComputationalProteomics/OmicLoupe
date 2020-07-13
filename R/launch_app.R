#' launches the shinyAppDemo app
#'
#' @export launchApp
#'
#' @return shiny application object
#'
#' @importFrom ggdendro theme_dendro segment label
#' @importFrom dplyr %>% mutate arrange pull group_by row_number summarize n inner_join mutate matches rename_all 
#' @importFrom dplyr rename_at all_of any_vars mutate_if all_of desc summarize_at
#' @importFrom forcats fct_collapse
#' @import ggplot2
#' @importFrom ggpubr ggarrange
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
launchApp <- function() {
    
    options(shiny.maxRequestSize=100*1024^2)
    ggplot2::theme_set(ggplot2::theme_classic())
    
    shinyApp(ui = get_ui(), server = get_server())
}


if(getRversion() >= "2.15.1")  utils::globalVariables(c("."))

