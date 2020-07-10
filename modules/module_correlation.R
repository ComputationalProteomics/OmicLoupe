setup_correlation_ui <- function(id) {
    ns <- NS(id)
    tabPanel(
        id,
        fluidPage(
            bar_w_help("Correlation", ns("help")),
            fluidRow(
                column(
                    12,
                    wellPanel(
                        sliderInput(ns("pthres"), "P-value threshold", min=0, max=1, value=0.05),
                        sliderInput(ns("fdrthres"), "FDR threshold", min=0, max=1, value=0.05),
                        downloadButton(ns("ggplot_download"), "Download static")
                    )
                )
            ),
            fluidRow(
                plotOutput(ns("correlation_histograms"))
            )
        )
    )
}

module_correlation_server <- function(input, output, session, rv, module_name) {
    
    output$ggplot_download <- downloadHandler(
        filename = function() {
            sprintf("%s-%s.%s", "corr", Sys.Date(), rv$figure_save_format())
        },
        content = function(file) {
            dpi <- rv$figure_save_dpi()
            ggsave(
                file, 
                plot = correlation_histograms(),
                width = rv$figure_save_width() / dpi, 
                height = rv$figure_save_height() / dpi, 
                units = "in", 
                dpi = dpi)
        }
    )
    
    observeEvent(input$help, {
        shinyalert(
            title = "Help: Correlation visuals",
            text = help_correlation, 
            html = TRUE
        )
    })
    
    correlation_histograms <- function() {
        comb_df <- rv$mapping_obj()$get_combined_dataset(full_entries=FALSE)
        make_corr_hist <- function(target_df, corr_base, title, has_sig=FALSE, bins=100) {
            
            not_sig_string <- "Not sig."
            p_level_string <- sprintf("P<%s", input$pthres)
            fdr_level_string <- sprintf("FDR<%s", input$fdrthres)
            
            cor_str <- sprintf("%s.cor", corr_base)
            if (has_sig) {
                p_str <- sprintf("%s.pval", corr_base)
                fdr_str <- sprintf("%s.fdr", corr_base)
                target_df$sig_type <- ifelse(
                    target_df[[fdr_str]] < input$fdrthres, 
                    fdr_level_string, 
                    ifelse(target_df[[p_str]] < input$pthres, 
                           p_level_string, 
                           "Not sig.")
                ) %>% factor(levels=c("Not sig.", p_level_string, fdr_level_string))
            }
            else {
                target_df$sig_type <- "NA"
            }
            
            mean_corr <- mean(target_df[[cor_str]], na.rm=TRUE)
            ggplot(target_df, aes_string(x=cor_str, fill="sig_type")) +
                geom_histogram(bins=bins, na.rm=TRUE) +
                geom_vline(xintercept = mean_corr, na.rm=TRUE) +
                ggtitle(sprintf("%s (mean %s)", title, round(mean_corr, 3))) +
                xlim(-1, 1) + 
                xlab("Correlation") + 
                ylab("Count") + 
                scale_fill_manual(values=setNames(c("#aaaaaa", "#3399FF", "#FF3333"), c(not_sig_string, p_level_string, fdr_level_string)))
        }
        
        ggpubr::ggarrange(
            make_corr_hist(comb_df, "d2.pearson", "Pearson", has_sig=TRUE),
            make_corr_hist(comb_df, "d2.spearman", "Spearman", has_sig=TRUE),
            make_corr_hist(comb_df, "d2.kendall", "Kendall", has_sig=TRUE),
            nrow=3
        )
    }
    
    output$correlation_histograms <- renderPlot({
        
        validate(need(!is.null(rv$mapping_obj()), "No mapping object found, is data loaded and samples mapped under the Setup page?"))
        validate(need(rv$mapping_obj()$has_correlations(), "No correlation object found! At the Setup page, do you have two matched datasets and the checkbox 'Skip correlation' unchecked?"))
        correlation_histograms()

    }, height = 800)
}


