setup_overlap_ui <- function(id) {
    ns <- NS(id)
    tabPanel(
        id,
        fluidPage(
            bar_w_help("Overlap study", ns("help")),
            fluidRow(
                column(
                    12,
                    wellPanel(
                        fluidRow(
                            column(6, selectInput(ns("dataset1"), "Reference dataset", choices = c("Dev"), selected = "Dev")),
                            column(6, selectInput(ns("dataset2"), "Comp. dataset", choices = c("Dev"), selected = "Dev"))
                        ),
                        fluidRow(
                            column(6, selectInput(ns("ref_contrast"), "Ref. contr.", choices = c("Dev"), selected = "Dev")),
                            column(6, selectInput(ns("comp_contrast"), "Comp. contr.", choices = c("Dev"), selected = "Dev"))
                        ),
                        fluidRow(
                            column(6, sliderInput(ns("stat_threshold"), "Stat. threshold", min=0, max=1, step=0.01, value=0.05)),
                            column(6, selectInput(ns("stat_contrast_type"), "Stat. contrast type", choices=c("P.Value", "adj.P.Val")))
                        ),
                        fluidRow(
                            column(6, sliderInput(ns("fold_threshold"), "Fold threshold", min=0, max=10, step=0.1, value=1)),
                            column(6, checkboxInput(ns("use_fold_cutoff"), "Use fold cutoff", value=FALSE))
                        ),
                        fluidRow(
                            column(6, conditionalPanel(
                                sprintf("input['%s'] == 'Venn'", ns("plot_tabs")),
                                selectInput(ns("select_target"), "Select target", choices=c("A", "B", "A&B", "A|B"), selected = "A&B")
                            ))
                        ),
                        conditionalPanel(
                            sprintf("input['%s'] == 'Upset' || input['%s'] == 'FoldComparison'", ns("plot_tabs"), ns("plot_tabs")),
                            selectInput(ns("upset_ref_comparisons"), "Upset ref. choices", choices = c("Dev"), selected="Dev", multiple = TRUE),
                            conditionalPanel(
                                sprintf("input['%s'] != input['%s']", ns("dataset1"), ns("dataset2")),
                                selectInput(ns("upset_comp_comparisons"), "Upset comp. choices", choices = c("Dev"), selected="Dev", multiple = TRUE)
                            )
                        ),
                        conditionalPanel(
                            sprintf("input['%s'] == 'Upset'", ns("plot_tabs")),
                            fluidRow(
                                column(6, numericInput(ns("upset_max_comps"), "Upset max comparisons (rows)", min = 1, value = 10)),
                                column(6, numericInput(ns("upset_max_intersects"), "Upset max intersects (columns)", min = 1, value = 40))
                            ),
                            fluidRow(
                                column(6, checkboxInput(ns("fold_split_upset"), "Fold split upset", value=FALSE)),
                                column(6, checkboxInput(ns("upset_degree_order"), "Order on degree", value=FALSE))
                            )
                        ),
                        conditionalPanel(
                            sprintf("input['%s'] == 'FoldComparison'", ns("plot_tabs")),
                            numericInput(ns("max_fold_comps"), "Max fold comps", min=1, value=10)
                        )
                    ),
                    htmlOutput(ns("warnings")),
                    conditionalPanel(
                        sprintf("input['%s'] == 'Upset'", ns("plot_tabs")),
                        wellPanel(
                            h3("Select overlap"),
                            selectInput(ns("upset_crosssec_display"), "Display cross-section", choices = c("Dev"), selected="Dev", multiple = TRUE),
                            textOutput(ns("test_crosssec_display"))
                        )
                    ),
                    tabsetPanel(
                        id = ns("plot_tabs"),
                        type = "tabs",
                        tabPanel("Venn",
                                 fluidRow(
                                     column(6, plotOutput(ns("venn"))),
                                     column(6, plotOutput(ns("fold_fractions_among_sig")))
                                 ),
                                 downloadButton(ns("download_table"), "Download table"),
                                 DT::DTOutput(ns("table_display"))
                        ),
                        tabPanel("Upset",
                                 plotOutput(ns("upset"), height = 800),
                                 DT::DTOutput(ns("table_display_upset"))
                        ),
                        tabPanel("FoldComparison",
                                 plotOutput(ns("fold_comp"))
                        )
                    )
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

module_overlap_server <- function(input, output, session, rv, module_name) {
    
    output$download_table <- downloadHandler(
        filename = function() {
            paste("overlap-", Sys.Date(), ".tsv", sep="")
        },
        content = function(file) {
            write_tsv(rv$dt_parsed_data_raw(rv, output_table_reactive()), file)
        }
    )
    
    observeEvent(input$help, {
        shinyalert(
            title = "Help: Overlap visuals",
            text = help_overlap, 
            html = TRUE
        )
    })
    
    parsed_overlap_entries <- reactive({
        parsed_ref_comps <- input$upset_ref_comparisons %>% gsub("\\.$", "", .)
        if (input$fold_split_upset) {
            parsed_ref_comps <- c(
                paste(parsed_ref_comps, "up", sep="."),
                paste(parsed_ref_comps, "down", sep=".")
            )
        }
        
        if (input$dataset1 == input$dataset2) {
            parsed_ref_comps
        }
        else {
            parsed_comp_comps <- input$upset_comp_comparisons %>% gsub("\\.$", "", .)
            if (input$fold_split_upset) {
                parsed_comp_comps <- c(
                    paste(parsed_comp_comps, "up", sep="."),
                    paste(parsed_comp_comps, "down", sep=".")
                )
            }
            parsed_combined_comps <- c(
                paste("d1", parsed_ref_comps, sep="."),
                paste("d2", parsed_comp_comps, sep=".")
            )
            parsed_combined_comps
        }
    })
    
    observeEvent(parsed_overlap_entries(), {
        updateSelectInput(session, "upset_crosssec_display", choices=parsed_overlap_entries(), selected = parsed_overlap_entries())
    })
    
    selected_id_reactive <- reactive({
        output_table_reactive()[input$table_display_rows_selected, ]$comb_id %>% as.character()
    })
    
    observeEvent(input$table_display_rows_selected, {
        rv$set_selected_feature(selected_id_reactive(), module_name)
    })
    
    ref_pass_reactive <- reactive({
        parse_contrast_pass_list(rv, input, input$dataset1, input$ref_contrast, input$stat_contrast_type)
    })
    
    comp_pass_reactive <- reactive({
        parse_contrast_pass_list(rv, input, input$dataset2, input$comp_contrast, input$stat_contrast_type)
    })
    
    upset_selected_ids <- reactive({
        
    })
    
    output_table_reactive <- reactive({
        
        validate(need(!is.null(rv$mapping_obj()), "No mapping object found, are samples mapped at the Setup page?"))
        validate(need(!is.null(rv$mapping_obj()$get_combined_dataset()), "No combined dataset found, are samples mapped at the Setup page?"))
        
        # browser()
        
        if (input$plot_tabs == "Venn") {
            ref_pass <- names(ref_pass_reactive())
            comp_pass <- names(comp_pass_reactive())
            
            if (input$select_target == "A&B") {
                target_ids <- union(ref_pass, comp_pass)
            }
            else if (input$select_target == "A|B") {
                target_ids <- intersect(ref_pass, comp_pass)
            }
            else if (input$select_target == "A") {
                target_ids <- setdiff(ref_pass, comp_pass)
            }
            else if (input$select_target == "B") {
                target_ids <- setdiff(comp_pass, ref_pass)
            }
            else {
                stop(sprintf("Unknown input$select_target: %s", input$select_target))
            }
        }
        else if (input$plot_tabs == "Upset") {
            plot_list <- upset_plot_list()
            set_ordering <- get_ordered_sets(UpSetR::fromList(plot_list), order_on = upset_order_by())
            crosssection_target_ordered <- input$upset_crosssec_display[order(match(input$upset_crosssec_display, upset_order_by()))]
            input$upset_crosssec_display
            
            all_contrasts <- UpSetR::fromList(plot_list) %>% colnames()
            
            # browser()
            
            
            
            all_genes <- plot_list %>% unlist() %>% unique()
            my_function <- function(x, all_features){
                is.element(all_features, x)
            }
            
            parsed_w_rownames <- as.data.frame(lapply(plot_list, my_function, all_features=all_genes))
            rownames(parsed_w_rownames) <- all_genes
            # test <- as.data.frame(lapply(my_sets, my_function))
            parsed_w_rownames[parsed_w_rownames=="TRUE"]<- 1
            parsed_w_rownames[parsed_w_rownames=="FALSE"]<- 0
            
            
            
            
            
            
            
            non_selected_contrasts <- c(all_contrasts[!all_contrasts %in% input$upset_crosssec_display])
            filtered_contrast_matrix <- parsed_w_rownames %>% 
                rownames_to_column("id_col") %>%
                filter_at(vars(all_of(input$upset_crosssec_display)), ~.==1)

            if (length(non_selected_contrasts) > 0) {
                filtered_contrast_matrix <- filtered_contrast_matrix %>% filter_at(vars(all_of(non_selected_contrasts)), ~.==0)
            }
            target_ids <- filtered_contrast_matrix %>% pull(id_col)
        }
        else {
            stop("input$plot_tabs should be either Venn or Upset, found: ", input$plot_tabs)
        }
        
        rv$mapping_obj()$get_combined_dataset() %>%
            filter(comb_id %in% target_ids)
    })
    
    upset_plot_list <- reactive({

        ref_names_list <- upset_extract_set_names_list(rv, input, input$upset_ref_comparisons, input$dataset1, input$stat_contrast_type, input$fold_split_upset)
        plot_list <- upset_get_plot_list(ref_names_list, input$upset_ref_comparisons, input$fold_split_upset)
        if (input$dataset1 != input$dataset2) {
            comp_names_list <- upset_extract_set_names_list(rv, input, input$upset_comp_comparisons, input$dataset2, input$stat_contrast_type, input$fold_split_upset)
            plot_list_comp <- upset_get_plot_list(comp_names_list, input$upset_comp_comparisons, input$fold_split_upset)
            plot_list <- c(
                plot_list %>% `names<-`(paste("d1", names(plot_list), sep=".")), 
                plot_list_comp %>% `names<-`(paste("d2", names(plot_list_comp), sep="."))
            )
        }
        plot_list
    })
    
    upset_name_order <- reactive({
        plot_list <- upset_plot_list()
        name_order <- upset_get_name_order(plot_list, input$fold_split_upset)
        if (input$dataset1 != input$dataset2) {
            
            name_order <- c(
                paste("d1", name_order, sep="."), 
                paste("d2", upset_get_name_order(plot_list_comp, input$fold_split_upset), sep=".")
            )
        }
        name_order
    })
    
    upset_metadata <- reactive({
        plot_list <- upset_plot_list()
        upset_metadata_obj <- upset_get_metadata(plot_list, input$fold_split_upset)
        if (input$dataset1 != input$dataset2) {
            if (!input$fold_split_upset) {
                metadata <- data.frame(
                    comparison = names(plot_list),
                    data_source = names(plot_list) %>% gsub("\\..*", "", .)
                )
                color_vector <- c(d1 = "navy", d2 = "orange")
            }
            else {
                metadata <- data.frame(
                    comparison = names(plot_list),
                    data_source = names(plot_list) %>% gsub("\\..*\\.", "\\.", .)
                )
                color_vector <- c(d1.up="red", d1.down="navy", d2.up="orange", d2.down="darkgreen")
            }
            upset_metadata_obj <- list(
                data = metadata,
                plots = list(list(
                    type = "matrix_rows",
                    column = "data_source",
                    colors = color_vector,
                    alpha=0.2
                ))
            )
        }
    })
    
    upset_order_by <- reactive({
        if (input$upset_degree_order) {
            "degree"
        }
        else {
            "freq"
        }
    })
    
    output$upset <- renderPlot({
        
        plot_list <- upset_plot_list()
        name_order <- upset_name_order()
        upset_metadata_obj <- upset_metadata()
        
        # if (input$upset_degree_order) {
        #     upset_order_by <- "degree"
        # }
        # else {
        #     upset_order_by <- "freq"
        # }
        
        validate(need(
            length(plot_list) > 1, 
            sprintf(sprintf("Number of contrasts need to be more than one, found: %s", length(plot_list)))
        ))
        
        if ("plots" %in% names(upset_metadata_obj)) {
            target_metadata <- upset_metadata_obj
        }
        else {
            target_metadata <- NULL
        }
        
        set_ordering <- get_ordered_sets(UpSetR::fromList(plot_list), order_on = upset_order_by())
        crosssection_target_ordered <- input$upset_crosssec_display[order(match(input$upset_crosssec_display, name_order))]
        bar_coloring <- (set_ordering$string_entries == paste(crosssection_target_ordered, collapse=",")) %>% ifelse("#298ff5", "gray23")
        
        UpSetR::upset(
            UpSetR::fromList(plot_list), 
            set.metadata=target_metadata,
            order.by=upset_order_by(), 
            sets=name_order,
            keep.order=TRUE,
            text.scale=2, 
            nsets=input$upset_max_comps,
            nintersects=input$upset_max_intersects,
            main.bar.color=bar_coloring
        )
    }, height = 800)
    
    output$fold_comp <- renderPlot({
        
        ref_names_list <- lapply(input$upset_ref_comparisons, function(stat_pattern, dataset, contrast_type) {
            parse_contrast_pass_list(rv, input, dataset, stat_pattern, contrast_type) %>% names()
        }, dataset=input$dataset1, contrast_type=input$stat_contrast_type)
        
        if (input$dataset1 != input$dataset2) {
            comp_names_list <- lapply(input$upset_comp_comparisons, function(stat_pattern, dataset, contrast_type) {
                parse_contrast_pass_list(rv, input, dataset, stat_pattern, contrast_type) %>% names()
            }, dataset=input$dataset2, contrast_type=input$stat_contrast_type)
            
            plot_list <- c(ref_names_list, comp_names_list)
            names(plot_list) <- c(
                paste("d1", input$upset_ref_comparisons, sep="."),
                paste("d2", input$upset_comp_comparisons, sep=".")
            )
        }
        else {
            plot_list <- ref_names_list
            names(plot_list) <- input$upset_ref_comparisons
        }
        
        present_in_all <- Reduce(intersect, plot_list)

        contrast_pval_cols_ref <- map(input$upset_ref_comparisons, ~rv$statcols_ref(rv, input$dataset1, contrast_field = .)$P.Value) %>% unlist()
        contrast_fold_cols_ref <- map(input$upset_ref_comparisons, ~rv$statcols_ref(rv, input$dataset1, contrast_field = .)$logFC) %>% unlist()
        contrast_pval_cols_comp <- map(input$upset_comp_comparisons, ~rv$statcols_comp(rv, input$dataset2, contrast_field = .)$P.Value) %>% unlist()
        contrast_fold_cols_comp <- map(input$upset_comp_comparisons, ~rv$statcols_comp(rv, input$dataset2, contrast_field = .)$logFC) %>% unlist()
        
        if (input$dataset1 == input$dataset2) {
            
            long_df <- rv$mapping_obj()$get_combined_dataset() %>% 
                filter(comb_id %in% present_in_all) %>%
                mutate(
                    p_sum=rowSums(.[, contrast_pval_cols_ref, drop=FALSE]),
                    p_prod=rowSums(.[, contrast_pval_cols_ref, drop=FALSE])
                ) %>%
                    arrange(p_sum) %>%
                    head(input$max_fold_comps) %>%
                    dplyr::select(ID=comb_id, p_sum=p_sum, contrast_fold_cols_ref
                ) %>%
                tidyr::gather("Comparison", "Fold", -ID, -p_sum)
        }
        else {
            long_df <- rv$mapping_obj()$get_combined_dataset() %>% 
                filter(comb_id %in% present_in_all) %>%
                mutate(
                    p_sum=rowSums(.[, c(contrast_pval_cols_ref, contrast_pval_cols_comp), drop=FALSE]),
                    p_prod=rowSums(.[, c(contrast_pval_cols_ref, contrast_pval_cols_comp), drop=FALSE])
                ) %>%
                    arrange(p_sum) %>%
                    head(input$max_fold_comps) %>%
                    dplyr::select(ID=comb_id, p_sum=p_sum, contrast_fold_cols_ref, contrast_fold_cols_comp
                ) %>%
                tidyr::gather("Comparison", "Fold", -ID, -p_sum)
        }
        
        plt <- ggplot(long_df, aes(x=reorder(ID, p_sum), y=Fold)) + theme_classic() +
            theme(axis.text.x = element_text(angle=90, vjust=0.5)) +
            geom_boxplot() +
            geom_point(aes(color=Comparison)) + 
            xlab("") +
            ggtitle(sprintf("%s out of %s features present in all shown", 
                            min(input$max_fold_comps, length(present_in_all)), 
                            length(present_in_all)
            ))
        plt
    })
    
    output$fold_fractions_among_sig <- renderPlot({
        
        validate(need(!is.null(rv$mapping_obj()), "No loaded data found, is everything set up at the Setup page?"))
        
        combined_dataset <- rv$mapping_obj()$get_combined_dataset(full_entries=FALSE)
        
        plot_df <- data.frame(
            ref_sig = combined_dataset[[rv$statcols_ref(rv, input$dataset1, input$ref_contrast)$P.Value]],
            ref_fold = combined_dataset[[rv$statcols_ref(rv, input$dataset1, input$ref_contrast)$logFC]],
            comp_sig = combined_dataset[[rv$statcols_comp(rv, input$dataset2, input$comp_contrast)$P.Value]],
            comp_fold = combined_dataset[[rv$statcols_comp(rv, input$dataset2, input$comp_contrast)$logFC]]
        ) %>% 
            mutate(highest_p=pmax(ref_sig, comp_sig)) %>% 
            arrange(highest_p) %>%
            mutate(is_contra=sign(ref_fold) != sign(comp_fold)) %>%
            mutate(tot_contra=cumsum(is_contra), tot_same=cumsum(!is_contra)) %>%
            mutate(cum_frac_contra=tot_same/(tot_same+tot_contra))
        
        plt_cumfrac_over_logp <- ggplot(plot_df, aes(x=log10(highest_p), y=cum_frac_contra)) + geom_line()
        plt_cumfrac_over_p <- ggplot(plot_df, aes(x=highest_p, y=cum_frac_contra)) + geom_line()
        
        ggarrange(plt_cumfrac_over_p, plt_cumfrac_over_logp, ncol=1, nrow=2) %>% ggpubr::annotate_figure(., top="Fraction same fold for different p-value thresholds")
        # ggarrange(plt_full, plt_subset, plt_cumfrac_over_logp, plt_cumfrac_over_p, ncol=2, nrow=2)
    })
    
    output$table_display <- output$table_display_upset <- DT::renderDataTable({
        rv$dt_parsed_data(rv, output_table_reactive())
    })
    
    output$venn <- renderPlot({
        
        venn$do_paired_expression_venn(
            ref_pass_reactive(), 
            comp_pass_reactive(), 
            title="", 
            highlight = input$select_target)
    })
    
    observeEvent(rv$filedata_1(), {
        choices <- get_dataset_choices(rv)
        updateSelectInput(session, "dataset1", choices=choices, selected=choices[1])
        updateSelectInput(session, "dataset2", choices=choices, selected=choices[1])
    })
    
    observeEvent(rv$filedata_2(), {
        choices <- get_dataset_choices(rv)
        updateSelectInput(session, "dataset1", choices=choices, selected=choices[1])
        updateSelectInput(session, "dataset2", choices=choices, selected=choices[1])
    })
    
    observeEvent({
        rv$selected_cols_obj() 
        input$dataset1 
        input$dataset2}, {
            req(rv$filename_1())

            choices_1 <- rv$selected_cols_obj()[[input$dataset1]]$statpatterns
            updateSelectInput(session, "ref_contrast", choices=choices_1, selected=choices_1[1])
            updateSelectInput(session, "upset_ref_comparisons", choices=choices_1, selected=choices_1)
            
            choices_2 <- rv$selected_cols_obj()[[input$dataset2]]$statpatterns
            updateSelectInput(session, "comp_contrast", choices=choices_2, selected=choices_2[1])
            updateSelectInput(session, "upset_comp_comparisons", choices=choices_2, selected=choices_2)
        })
}

