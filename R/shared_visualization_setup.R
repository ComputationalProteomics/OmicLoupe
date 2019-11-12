
data_display_ui_panel <- function(ns) {
    
    wellPanel(
        selectInput(ns("color_type"), "Coloring type", choices=c("select", "threshold", "PCA loading"), selected="select"),
        column(6,
               selectInput(ns("dataset1"), "Reference dataset", choices=c(), selected = ""),
               selectInput(ns("dataset2"), "Compare dataset", choices=c(), selected = "")
        ),
        column(6,
               selectInput(ns("stat_base1"), "Ref. Comparison", choices=c(), selected = ""),
               selectInput(ns("stat_base2"), "Comp. Comparison", choices=c(), selected = "")
        ),
        # selectInput(ns("reference_dataset"), "Reference dataset", choices=c(), selected = "dataset1"),
        fluidRow(
            column(9,
                   sliderInput(ns("pvalue_cutoff"), "P-value cutoff", value=0.05, step=0.01, min=0, max=1)
            ),
            column(3,
                   span(
                       selectInput(ns("pvalue_type_select"), choices = c("P.Value", "adj.P.Val"), selected = "P.Value", label = "P-value type"),
                       style="padding:20px"
                   )
            )
        ),
        sliderInput(ns("fold_cutoff"), "Fold cutoff", value=1, step=0.1, min=0, max=10),
        sliderInput(ns("bin_count"), "Bin count", value=50, step=10, min=10, max=200),
        checkboxInput(ns("toggle_minimaps"), "Display minimaps", value=FALSE)
    )
}

minimaps_panel <- function(ns) {
    conditionalPanel(
        sprintf("input['%s'] == 1", ns("toggle_minimaps")),
        plotOutput(ns("mini_pvalue_dist"), height = 200),
        plotOutput(ns("mini_fold_dist"), height = 200)
    )
}

parse_stat_cols_for_visuals <- function(rv_filename1, rv_filename2, rv_selected_cols_obj, selected_dataset, selected_statbase) {
    # parse_stat_cols_for_visuals <- function(reactive_vals, selected_dataset, selected_statbase) {
        
    # filenames <- c(
    #     reactive_vals$filename_1(),
    #     reactive_vals$filename_2()
    # )
    filenames <- c(
        rv_filename1,
        rv_filename2
    )
    
    selection_index <- which(filenames %in% selected_dataset)
    
    statcols <- rv_selected_cols_obj[[selected_dataset]]$statcols
    # statcols <- reactive_vals$selected_cols_obj()[[selected_dataset]]$statcols
    parsed_cols <- parse_stat_cols(statcols, selected_statbase)
    d_parsed_cols <- lapply(parsed_cols, function(elem) { sprintf("d%s.%s", selection_index, elem) })
    d_parsed_cols
}

get_dataset_choices <- function(reactive_vals) {
    choices <- c()
    if (!is.null(reactive_vals$filedata_1())) {
        choices <- c(choices, reactive_vals$filename_1())
    }
    if (!is.null(reactive_vals$filedata_2())) {
        choices <- c(choices, reactive_vals$filename_2())
    }
    choices
}

update_stat_inputs <- function(session, reactive_vals, dataset1_select, dataset2_select) {
    if (is.null(reactive_vals$filename_1()) && is.null(reactive_vals$filename_2())) {
        return()
    }
    
    choices_1 <- reactive_vals$selected_cols_obj()[[dataset1_select]]$statpatterns
    choices_2 <- reactive_vals$selected_cols_obj()[[dataset2_select]]$statpatterns
    
    updateSelectInput(session, "stat_base1", choices=choices_1, selected=choices_1[1])
    updateSelectInput(session, "stat_base2", choices=choices_2, selected=choices_2[1])
}

















