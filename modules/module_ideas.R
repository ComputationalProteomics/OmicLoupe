setup_ideas_ui <- function(id) {
    ns <- NS(id)
    tabPanel(
        id,
        fluidPage(
            fluidRow(
                column(8,
                       h4("Some ideas for development"),
                       htmlOutput(ns("html"))
                )
            )
        )
    )
}

module_ideas_server <- function(input, output, session) {
    
    output$html <- renderUI({
        
        entries <- c(
            "Could consider allowing sample-matched comparisons, which would enable direct comparison in for instance PCA plot and calculating correlations",
            "More advanced ID mapping system would be beneficial (now required to uniquely match)",
            "(Needed) more flexible way to assign statistical columns",
            "For volcano / MA - Allow fixing x/y axis to same range for all four plots",
            "Could use hexbins for summarizing scatter distribution when number of datapoints is huge",
            "Could do heatmap to illustrate feature expression / missing values",
            "(Needed) Loading screen when calculating the plots",
            "Alternative view for scatter plot: Full screen one of the plots",
            "Think about documentation: Work through the help page, and do video at some point",
            "PCA: Allow filtering to view subsets of PCA in efficient way (can be done by sample selection, but could be more useful)",
            "PCA: Illustrating PC distributions",
            "PCA: Integrate more closely with scatter distribution by showing at same page",
            "Performing Hive plot or Circular plot as 'master illustration' overviewing the two datasets globally",
            "Do measures of similarity across datasets, such as correlation of fold changes or p-value orders",
            "PCA: Facet view!",
            "Think about error messages, making sure they are clear, coherent and timely",
            "Consider if limited unit testing is needed",
            "Is a full 'table page' needed?",
            "Plotly: Could allow selecting additional annotation information column for hover",
            "Think: How should sample mapping be performed? Design matrix, manual clicking or both (as now)?",
            "Would a dashboard be beneficial for clarification? (Not as it looks now I think)"
        )
        
        html_string <- paste0(
            "<li>",
            paste(entries, collapse="</li><li>"),
            "</li>"
        )
        
        HTML(sprintf("<ul>%s</ul>", html_string))
    })
}

