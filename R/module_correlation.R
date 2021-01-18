setup_correlation_ui <- function(id) {
    ns <- shiny::NS(id)
    tabPanel(
        id,
        fluidPage(
            bar_w_help_and_download("Correlation", ns("help"), ns("download_settings"), ns("download_report")),
            fluidRow(
                column(
                    12,
                    wellPanel(
                        sliderInput(ns("pthres"), "P-value threshold", min=0, max=1, value=0.05),
                        sliderInput(ns("fdrthres"), "FDR threshold", min=0, max=1, value=0.05),
                        fluidRow(
                            column(4, checkboxInput(ns("show_pearson"), "Show Pearson", value = TRUE)),
                            column(4, checkboxInput(ns("show_spearman"), "Show Spearman", value = TRUE)),
                            column(4, checkboxInput(ns("show_kendall"), "Show Kendall", value = FALSE))
                        ),
                        checkboxInput(ns("more_settings"), "More settings", value=FALSE),
                        conditionalPanel(
                            sprintf("input['%s'] == 1", ns("more_settings")),
                            downloadButton(ns("ggplot_download"), "Download static"),
                            numericInput(ns("font_size"), "Font size", value=14, min=0)
                        )
                        
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
            sprintf("%s-%s.%s", "corr", format(Sys.time(), "%y%m%d_%H%M%S"), rv$figure_save_format())
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
    
    output$download_settings <- settings_download_handler("corr", input)
    
    output$download_report <- report_generation_handler("corr", params=list(
        input=input,
        setup_input=rv$setup_input(),
        make_correlation_plot=correlation_histograms
    ))
    
    observeEvent(input$help, {
        shinyalert(
            title = "Help: Correlation visuals",
            text = help_correlation, 
            html = TRUE
        )
    })

    make_corr_hist <- function(target_df, corr_base, title, has_sig=FALSE, bins=100, font_size=10) {
        
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
        median_corr <- median(target_df[[cor_str]], na.rm=TRUE)
        ggplot(target_df, aes_string(x=cor_str, fill="sig_type")) +
            geom_histogram(bins=bins, na.rm=TRUE) +
            geom_vline(xintercept = mean_corr, na.rm=TRUE) +
            ggtitle(sprintf("%s (median %s, mean %s)", title, round(median_corr, 3), round(mean_corr, 3))) +
            xlim(-1, 1) + 
            xlab("Correlation") + 
            ylab("Count") + 
            scale_fill_manual(values=setNames(c("#aaaaaa", "#3399FF", "#FF3333"), c(not_sig_string, p_level_string, fdr_level_string))) +
            theme(text=element_text(size=font_size))
    }
    
    correlation_histograms <- function() {
        comb_df <- rv$mapping_obj()$get_combined_dataset(only_no_na_entries=FALSE, include_one_dataset_entries=FALSE)
        out_plts <- list()
        if (input$show_pearson) {
            out_plts$pearson <- make_corr_hist(comb_df, "pearson", "Pearson", has_sig=TRUE, font_size=input$font_size)
        }
        if (input$show_spearman) {
            out_plts$spearman <- make_corr_hist(comb_df, "spearman", "Spearman", has_sig=TRUE, font_size=input$font_size)
        }
        if (input$show_kendall) {
            out_plts$kendall <- make_corr_hist(comb_df, "kendall", "Kendall", has_sig=TRUE, font_size=input$font_size)
        }

        ggpubr::ggarrange(plotlist=out_plts, nrow=length(out_plts))
    }
    
    output$correlation_histograms <- renderPlot({
        
        shiny::validate(need(!is.null(rv$mapping_obj()), "No mapping object found, is data loaded and samples mapped under the Setup page?"))
        shiny::validate(need(rv$mapping_obj()$has_correlations(), "No correlation object found! At the Setup page, do you have two matched datasets and the checkbox 'Skip correlation' unchecked?"))
        correlation_histograms()

    }, height = 800)
}


