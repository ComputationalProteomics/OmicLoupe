setup_ideas_ui <- function(id) {
    ns <- NS(id)
    tabPanel(
        id,
        fluidPage(
            fluidRow(
                column(8,
                       h4("Important points"),
                       htmlOutput(ns("important_points")),
                       h4("Priority points"),
                       htmlOutput(ns("priority_points")),
                       h4("Potential points"),
                       htmlOutput(ns("potential_points"))
                )
            )
        )
    )
}

parse_vector_to_bullets <- function(vect, number=TRUE) {
    html_string <- paste0(
        "<li>",
        paste(vect, collapse="</li><li>"),
        "</li>"
    )
    
    if (!number) {
        list_style <- "ul"
    }
    else {
        list_style <- "ol"
    }
    
    sprintf("<%s>%s</%s>", list_style, html_string, list_style)
}

module_ideas_server <- function(input, output, session, module_name) {
    
    output$important_points <- renderUI({
        
        HTML(parse_vector_to_bullets(c(
            "More flexible way to assign statistical columns",
            "Download tables",
            "Download figures buttons"
        )))
    })
    
    output$priority_points <- renderUI({
        HTML(parse_vector_to_bullets(c(
            "Could consider allowing sample-matched comparisons, which would enable direct comparison in for instance PCA plot and calculating correlations",
            "PCA: Allow filtering to view subsets of PCA in efficient way (can be done by sample selection, but could be more useful)",
            "PCA: Facet view!",
            "Think about error messages, making sure they are clear, coherent and timely",
            "Plotly: Could allow selecting additional annotation information column for hover",
            "Think: How should sample mapping be performed? Design matrix, manual clicking or both (as now)?",
            "For volcano / MA - Allow fixing x/y axis to same range for all four plots"
        )))
    })
    
    output$potential_points <- renderUI({
        HTML(parse_vector_to_bullets(c(
            "More advanced ID mapping system would be beneficial (now required to uniquely match)",
            "Could use hexbins for summarizing scatter distribution when number of datapoints is huge",
            "Could do heatmap to illustrate feature expression / missing values",
            "Alternative view for scatter plot: Full screen one of the plots",
            "Think about documentation: Work through the help page, and do video at some point",
            "PCA: Illustrating PC distributions",
            "PCA: Integrate more closely with scatter distribution by showing at same page",
            "Performing Hive plot or Circular plot as 'master illustration' overviewing the two datasets globally",
            "Do measures of similarity across datasets, such as correlation of fold changes or p-value orders",
            "Consider if limited unit testing is needed"
        )))
    })
}

