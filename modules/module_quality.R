setup_quality_ui <- function(id) {
    ns <- NS(id)
    tabPanel(
        id,
        fluidPage(
            fluidRow(
                column(8,
                       h4("Some ideas for development"),
                       htmlOutput(ns("html"))
                )
            ),
            tabsetPanel(
                type = "tabs",
                tabPanel("Boxplots", plotOutput(ns("boxplots"))),
                tabPanel("Density", plotOutput(ns("density"))),
                tabPanel("Barplots", plotOutput(ns("bars"))),
                tabPanel("Histograms", plotOutput(ns("histograms")))
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

module_quality_server <- function(input, output, session, rv) {
    
    output$html <- renderUI({
        
        HTML(parse_vector_to_bullets(c(
            "Density plots (colored from design)",
            "Box plots (colored from design)",
            "Total intensity / total missing (colored from design)",
            "Tree plot",
            "Histograms (intensity, retentiontime, whichever column)",
            "Design histograms (how target characteristic is distributed)"
        )))
    })
    
    output$bars <- renderPlot({ 
        
        dataset1 <- rv$mapping_obj()$dataset1
        sample_cols1 <- rv$mapping_obj()$samples1
        sdf1 <- dataset1[, sample_cols1]
        ddf1 <- rv$design_1()
        target_ddf_cond1 <- "variety"
        target_ddf_samplecol1 <- "old_sample"
        join_by <- c("name"=target_ddf_samplecol1)
        long_sdf1 <- sdf1 %>%
            pivot_longer(sample_cols1) %>%
            inner_join(ddf1, by=join_by)

        do_sum <- TRUE
        
        # Bar
        if (do_sum) {
            summed_data1 <- long_sdf1 %>% 
                group_by(name) %>%
                summarize_at("value", sum, na.rm=TRUE) %>% inner_join(ddf1, by=join_by)
        }
        else {
            summed_data1 <- long_sdf1 %>%
                mutate(value=is.na(value)) %>%
                group_by(name) %>%
                summarize_at("value", sum, na.rm=TRUE) %>% inner_join(ddf1, by=join_by)
        }
        
        plt1 <- ggplot(summed_data1, aes_string(x="name", y="value", fill=target_ddf_cond1)) + geom_col() + ggtitle("Barplot 1")
        plt2 <- ggplot(summed_data1, aes_string(x="name", y="value", fill=target_ddf_cond1)) + geom_col() + ggtitle("Barplot 2")
        
        return(ggarrange(plt1, plt2, nrow=2, ncol=1))
        
    })
    
    output$boxplots <- renderPlot({ 
        dataset1 <- rv$mapping_obj()$dataset1
        sample_cols1 <- rv$mapping_obj()$samples1
        sdf1 <- dataset1[, sample_cols1]
        ddf1 <- rv$design_1()
        target_ddf_cond1 <- "variety"
        target_ddf_samplecol1 <- "old_sample"
        join_by <- c("name"=target_ddf_samplecol1)
        long_sdf1 <- sdf1 %>%
            pivot_longer(sample_cols1) %>%
            inner_join(ddf1, by=join_by)
        
        do_violin <- TRUE
        plt1 <- ggplot(long_sdf1, aes_string(x="name", y="value", color=target_ddf_cond1)) + ggtitle("Boxplot 1")
        plt2 <- ggplot(long_sdf1, aes_string(x="name", y="value", color=target_ddf_cond1)) + ggtitle("Boxplot 2")
        
        if (!do_violin) {
            plt1 <- plt1 + geom_boxplot(na.rm=TRUE)
            plt2 <- plt2 + geom_boxplot(na.rm=TRUE)
        }
        else {
            plt1 <- plt1 + geom_violin(na.rm=TRUE)
            plt2 <- plt2 + geom_violin(na.rm=TRUE)
        }
        
        ggarrange(plt1, plt2, nrow=2, ncol=1)
    })
    
    output$density <- renderPlot({ 
        dataset1 <- rv$mapping_obj()$dataset1
        sample_cols1 <- rv$mapping_obj()$samples1
        sdf1 <- dataset1[, sample_cols1]
        ddf1 <- rv$design_1()
        target_ddf_cond1 <- "variety"
        target_ddf_samplecol1 <- "old_sample"
        join_by <- c("name"=target_ddf_samplecol1)
        long_sdf1 <- sdf1 %>%
            pivot_longer(sample_cols1) %>%
            inner_join(ddf1, by=join_by)
        
        plt1 <- ggplot(long_sdf1, aes_string(x="value", group="name", color=target_ddf_cond1)) + geom_density(na.rm=TRUE) + ggtitle("Density 1")
        plt2 <- ggplot(long_sdf1, aes_string(x="value", group="name", color=target_ddf_cond1)) + geom_density(na.rm=TRUE) + ggtitle("Density 2")
        
        ggarrange(plt1, plt2, nrow=2, ncol=1)
    })
    
    
    output$histograms <- renderPlot({ 
        
        rdf <- rv$mapping_obj()$dataset1
        
        adf_num_col <- "Arg_4d.P.Value"
        adf_color_col <- "pep_count"
        
        rdf[[adf_color_col]] <- as.factor(rdf[[adf_color_col]])
        
        color_freq_table <- table(rdf[[adf_color_col]])
        retain_count <- 3
        
        combine_names <- names(color_freq_table)[!names(color_freq_table) %in% names(sort(color_freq_table, decreasing = TRUE))[1:retain_count]]
        
        rdf[[adf_color_col]] <- rdf[[adf_color_col]] %>% fct_collapse(other=combine_names)
        
        plt1 <- ggplot(rdf, aes_string(x=adf_num_col, fill=adf_color_col)) + geom_histogram(na.rm=TRUE, bins=50) + ggtitle("Histogram 1")
        plt2 <- ggplot(rdf, aes_string(x=adf_num_col, fill=adf_color_col)) + geom_histogram(na.rm=TRUE, bins=50) + ggtitle("Histogram 2")
        
        ggarrange(plt1, plt2, nrow=2, ncol=1)    
    })
}


















