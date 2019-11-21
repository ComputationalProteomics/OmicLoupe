sample_input_well <- function(upload_id, select_col_id, feature_col_id, select_size=12) {
    wellPanel(
        fileInput(
            upload_id, 
            "Choose data file (TSV)",
            multiple = FALSE,
            accept = c("test/tsv", ".tsv")
        ),
        selectInput(
            select_col_id,
            "Select columns",
            choices = c("Upload a file", "to see colnames here"),
            multiple = TRUE,
            selectize = FALSE,
            size = select_size
        ),
        selectInput(
            feature_col_id,
            "Feature column",
            choices = c(""),
            multiple = FALSE,
            selectize = FALSE
        )
    )
}

design_input_well <- function(design_upload_id, sample_col_id) {
    wellPanel(
        fileInput(
            design_upload_id, 
            "Choose design file (TSV) (optional)",
            multiple = FALSE,
            accept = c("test/tsv", ".tsv")
        ),
        selectInput(
            sample_col_id,
            "Select sample column",
            choices = c(""),
            multiple = FALSE,
            selectize = FALSE
        )
    )
}

select_button_row <- function(title, select_button_id, deselect_button_id) {
    fluidRow(
        class = "button_row",
        h4(title),
        actionButton(
            deselect_button_id,
            width = "30%",
            "<"
        ),
        actionButton(
            select_button_id,
            width = "30%",
            ">"
        )
    )
}

action_button_row <- function(id, label) {
    fluidRow(
        class = "button_row",
        actionButton(
            id,
            width = "80%",
            label
        )
    )
}

informative_text <- function() {
    span(
        p("For now, statistical columns are expected to have the form: Base.Statistic
           and should contain the statistics 'P.Value', 'adj.P.Val', 'logFC' and 'AveExpr'
           If fullfilled, OmicLoupe figures out what 'Base' patterns are present"),
        p("Tip: You can mark multiple columns by pressing SHIFT and/or CONTROL when doing the selections")
    )
}
