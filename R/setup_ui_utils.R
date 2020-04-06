bar_w_help <- function(title, button_id) {
    fluidRow(
        span(
            style="display: inline-block; vertical-align:top; padding-right:10px; margin-top; -50px;", 
            h3(title)
        ),
        span(
            style="display: inline-block; vertical-align:top; width: 30px; padding-top:25px; padding-bottom:30px;", 
            actionButton(button_id, "", icon=icon("question"), style="padding-top:2px; font-size:70%;", class="btn-xs help")
        )
    )
}

top_bar_w_help <- function(title, button_id) {
    warning("top_bar_w_help is replaced with bar_w_help")
    bar_w_help(title, button_id)
}

sample_input_well <- function(upload_id, select_col_id, feature_col_id, annot_col_id, parsing_errors_id, select_size=12) {
    wellPanel(
        fileInput(
            upload_id, 
            "Choose data file (TSV)",
            # class = "file_browser_button",
            multiple = FALSE,
            accept = c("test/tsv", ".tsv")
        ),
        uiOutput(parsing_errors_id),
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
        ),
        selectInput(
            annot_col_id,
            "Annotation column",
            choices = c(""),
            multiple = FALSE,
            selectize = FALSE
        )
    )
}

design_input_well <- function(design_upload_id, sample_col_id, cond_col_id, parsing_errors_id) {
    wellPanel(
        fileInput(
            design_upload_id, 
            "Choose design file (TSV)",
            multiple = FALSE,
            accept = c("test/tsv", ".tsv")
        ),
        uiOutput(parsing_errors_id),
        selectInput(
            sample_col_id,
            "Sample column",
            choices = c(""),
            multiple = FALSE,
            selectize = FALSE
        ),
        selectInput(
            cond_col_id,
            "Default condition column",
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
