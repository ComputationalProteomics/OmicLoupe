source("R/setup_ui_utils.R")

setup_panel_ui <- function(id) {
    
    ns <- NS(id)
    tabPanel(
        id,
        fluidPage(
            id = "outer_area",
            tags$style(
                type = "text/css",
                ".button_row { padding: 5px; }",
                "#column_select_noselectize { height: 500px; }"
            ),
            fluidRow(
                column(4,
                       sample_input_well(ns("data_file_1"), ns("data_selected_columns_1")),
                       conditionalPanel(
                           sprintf("input['%s'] == 1", ns("toggle_dataset2")),
                           sample_input_well(ns("data_file_2"), ns("data_selected_columns_2"))
                       )
                ),  
                column(3,
                       align="center",
                       wellPanel(
                           select_button_row("Select samples", ns("sample_select_button_1"), ns("sample_deselect_button_1")),
                           select_button_row("Select stat groups", ns("stat_select_button_1"), ns("stat_deselect_button_1")),
                           select_button_row("Select samples 2", ns("sample_select_button_2"), ns("sample_deselect_button_2")),
                           select_button_row("Select stat groups 2", ns("stat_select_button_2"), ns("stat_deselect_button_2")),
                           action_button_row(ns("autodetect_stat_cols"), "Autodetect"),
                           action_button_row(ns("selection_clear_button"), "Clear selection"),
                           fluidRow(
                               class = "button_row",
                               column(6,
                                      checkboxInput(ns("toggle_dataset2"), label = "Toggle dataset 2", value = TRUE)
                               ),
                               column(6,
                                      checkboxInput(ns("autodetect_statcols_toggle"), label = "Autodetect", value = FALSE)
                               )
                           )
                       ),
                       wellPanel(
                           textOutput(ns("perform_map_status"))
                           # hr(),
                           # textInput(ns("pattern_pval"), "P-value pattern", value="P.Value"),
                           # textInput(ns("pattern_fdr"), "FDR pattern", value="adj.P.Val"),
                           # textInput(ns("pattern_fold"), "Log2-fold pattern", value="log2FC"),
                           # textInput(ns("pattern_aveexpr"), "Expression pattern", value="AveExpr"),
                           # checkboxInput(ns("joint_aveexpr"), "Joint average express", value=FALSE)
                       ),
                       informative_text()
                ),
                column(5,
                       selected_sample_well(
                           ns("sample_selected_1"),
                           ns("statcols_selected_1"),
                           ns("feature_col_1"),
                           ns("found_stat_patterns_1")
                       ),
                       conditionalPanel(
                           sprintf("input['%s'] == 1", ns("toggle_dataset2")),
                           selected_sample_well(
                               ns("sample_selected_2"),
                               ns("statcols_selected_2"),
                               ns("feature_col_2"),
                               ns("found_stat_patterns_2")
                           )
                       )
                )
            )
        )
    )
}


