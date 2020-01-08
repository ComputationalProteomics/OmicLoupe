setup_correlation_ui <- function(id) {
    ns <- NS(id)
    tabPanel(
        id,
        fluidPage(
            top_bar_w_help("Correlation", ns("help")),
            fluidRow(
                htmlOutput(ns("warnings")),
                plotOutput(ns("correlation_histograms"))
            )
        )
    )
}

module_correlation_server <- function(input, output, session, rv, module_name) {
    
    output$correlation_histograms <- renderPlot({
        
        comb_df <- rv$mapping_obj()$get_combined_dataset(full_entries=FALSE)
        
        make_corr_hist <- function(target_df, aes_x, title, bins=100) {
            mean_corr <- mean(target_df[[aes_x]], na.rm=TRUE)
            ggplot(target_df, aes_string(x=aes_x)) +
                geom_histogram(bins=bins, na.rm=TRUE) +
                geom_vline(xintercept = mean_corr, na.rm=TRUE) +
                ggtitle(sprintf("%s (mean %s)", title, round(mean_corr, 3))) +
                xlim(-1, 1)
        }

        ggpubr::ggarrange(
            make_corr_hist(comb_df, "d2.pearson", "Pearson"),
            make_corr_hist(comb_df, "d2.spearman", "Spearman"),
            make_corr_hist(comb_df, "d2.kendall", "Kendall"),
            nrow=3
        )
    }, height = 800)
}


