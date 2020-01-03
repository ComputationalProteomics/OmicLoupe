setup_correlation_ui <- function(id) {
    ns <- NS(id)
    tabPanel(
        id,
        fluidPage(
            fluidRow(
                column(8,
                       h4("Some ideas for development"),
                       htmlOutput(ns("html"))
                ),
                htmlOutput(ns("warnings")),
                plotOutput(ns("correlation_histograms"))
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

module_correlation_server <- function(input, output, session, rv) {
    
    output$correlation_histograms <- renderPlot({
        
        comb_df <- rv$mapping_obj()$get_combined_dataset(full_entries=F)
        
        ggpubr::ggarrange(
            ggplot(comb_df, aes(x=d2.pearson)) + geom_histogram(bins=100, na.rm=TRUE),
            ggplot(comb_df, aes(x=d2.spearman)) + geom_histogram(bins=100, na.rm=TRUE),
            ggplot(comb_df, aes(x=d2.kendall)) + geom_histogram(bins=100, na.rm=TRUE),
            nrow=3
        )
    }, height = 800)
    
    output$html <- renderUI({
        HTML(parse_vector_to_bullets(c(
            "Show correlation histograms",
            "Other correlation visualizations?"
        )))
    })
}


