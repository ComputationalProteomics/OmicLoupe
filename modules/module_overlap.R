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
                            column(6,
                                   selectInput(ns("dataset1"), "Reference dataset", choices = c("Dev"), selected = "Dev"),
                                   selectInput(ns("dataset2"), "Comp. dataset", choices = c("Dev"), selected = "Dev"),
                                   sliderInput(ns("threshold"), "Threshold", min=0, max=1, step=0.01, value=0.05),
                                   conditionalPanel(
                                       sprintf("input['%s'] == 'Venn'", ns("plot_tabs")),
                                       selectInput(ns("select_target"), "Select target", choices=c("A", "B", "A&B", "A|B"), selected = "A&B")
                                   )
                            ),
                            column(6,
                                   selectInput(ns("ref_contrast"), "Ref. contr.", choices = c("Dev"), selected = "Dev"),
                                   selectInput(ns("comp_contrast"), "Comp. contr.", choices = c("Dev"), selected = "Dev"),
                                   selectInput(ns("contrast_type"), "Contrast type", choices=c("P.Value", "adj.P.Val", "logFC"))
                            )
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
                            numericInput(ns("upset_max_comps"), "Upset comparison count", min = 1, value = 10),
                            checkboxInput(ns("fold_split_upset"), "Fold split upset", value=FALSE)
                        ),
                        conditionalPanel(
                            sprintf("input['%s'] == 'FoldComparison'", ns("plot_tabs")),
                            numericInput(ns("max_fold_comps"), "Max fold comps", min=1, value=10)
                        )
                    ),
                    htmlOutput(ns("warnings")),
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
                                 plotOutput(ns("upset"))
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
    
    observeEvent(input$contrast_type, {
        if (input$contrast_type != "logFC") {
            updateSliderInput(session, "threshold", min=0, max=1)
            # updateSelectInput(session, "dataset1", choices=choices, selected=choices[1])
        }
        else {
            comb_obj <- rv$mapping_obj()$get_combined_dataset()
            
            max_fold <- max(
                comb_obj[, paste0(sprintf("d%s.", di_new(rv, input$dataset1)), input$upset_ref_comparisons, "logFC")],
                comb_obj[, paste0(sprintf("d%s.", di_new(rv, input$dataset2)), input$upset_ref_comparisons, "logFC")],
                na.rm=TRUE
            )
            
            updateSliderInput(session, "threshold", min=0, max=ceiling(max_fold))
        }
    })
    
    selected_id_reactive <- reactive({
        output_table_reactive()[input$table_display_rows_selected, ]$comb_id %>% as.character()
    })
    
    observeEvent(input$table_display_rows_selected, {
        rv$set_selected_feature(selected_id_reactive(), module_name)
    })
    
    parse_contrast_pass_list <- function(target_data, target_contrast, contrast_type) {
        
        combined_dataset <- rv$mapping_obj()$get_combined_dataset(full_entries=FALSE)
        sig_field <- rv$statcols_ref(rv, target_data, target_contrast)[[contrast_type]]
        fold_field <- rv$statcols_ref(rv, target_data, target_contrast)$logFC
        
        if (contrast_type != "logFC") {
            pass_tbl <- combined_dataset %>% 
                dplyr::filter(UQ(as.name(sig_field)) < input$threshold)
        }
        else {
            pass_tbl <- combined_dataset %>% 
                dplyr::filter(abs(UQ(as.name(sig_field))) > input$threshold)
        }
        
        pass_tbl <- pass_tbl %>%
            dplyr::select(c("comb_id", fold_field)) %>%
            dplyr::rename(fold=fold_field) %>%
            mutate(comb_id=as.character(comb_id))
        
        pass_list <- setNames(as.list(pass_tbl$fold), pass_tbl$comb_id)
        pass_list
    }
    
    ref_pass_reactive <- reactive({
        parse_contrast_pass_list(input$dataset1, input$ref_contrast, input$contrast_type)
    })
    
    comp_pass_reactive <- reactive({
        parse_contrast_pass_list(input$dataset2, input$comp_contrast, input$contrast_type)
    })
    
    output_table_reactive <- reactive({
        
        req(rv$mapping_obj())
        req(rv$mapping_obj()$get_combined_dataset())
        
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
        
        rv$mapping_obj()$get_combined_dataset() %>%
            filter(comb_id %in% target_ids)
    })
    
    output$upset <- renderPlot({
        
        ref_names_list <- lapply(input$upset_ref_comparisons, function(stat_pattern, dataset, contrast_type, fold_split) {
            joint_entries_w_fold <- parse_contrast_pass_list(dataset, stat_pattern, contrast_type)
            if (!fold_split) {
                joint_entries_w_fold %>% names()
            }
            else {
                joint_up_features <- Filter(function(elem) { elem > 0 }, joint_entries_w_fold)
                joint_down_features <- Filter(function(elem) { elem < 0 }, joint_entries_w_fold)
                list(
                    up = joint_up_features %>% names(),
                    down = joint_down_features %>% names()
                )
            }
        }, dataset=input$dataset1, contrast_type=input$contrast_type, fold_split=input$fold_split_upset)
        
        if (input$dataset1 != input$dataset2) {
            
            comp_names_list <- lapply(input$upset_comp_comparisons, function(stat_pattern, dataset, contrast_type, fold_split) {
                joint_entries_w_fold <- parse_contrast_pass_list(dataset, stat_pattern, contrast_type)
                if (!fold_split) {
                    joint_entries_w_fold %>% names()
                }
                else {
                    joint_up_features <- Filter(function(elem) { elem > 0 }, joint_entries_w_fold)
                    joint_down_features <- Filter(function(elem) { elem < 0 }, joint_entries_w_fold)
                    list(
                        up = joint_up_features %>% names(),
                        down = joint_down_features %>% names()
                    )
                }
            }, dataset=input$dataset2, contrast_type=input$contrast_type, fold_split=input$fold_split_upset)
            
            if (!input$fold_split_upset) {
                plot_list <- c(ref_names_list, comp_names_list)
                names(plot_list) <- c(
                    paste("d1", input$upset_ref_comparisons, sep="."),
                    paste("d2", input$upset_comp_comparisons, sep=".")
                )
                metadata <- data.frame(
                    comparison = c(names(plot_list)),
                    data_source = c(
                        rep("d1", length(input$upset_ref_comparisons)),
                        rep("d2", length(input$upset_comp_comparisons))
                    )
                )
            }
            else {
                ref_pltlist <- ref_names_list
                comp_pltlist <- comp_names_list
                names(ref_pltlist) <- sprintf("d1.%s", input$upset_ref_comparisons %>% gsub("\\.$", "", .))
                names(comp_pltlist) <- sprintf("d2.%s", input$upset_comp_comparisons %>% gsub("\\.$", "", .))
                plot_list <- lapply(rapply(c(ref_pltlist, comp_pltlist), enquote, how="unlist"), eval)
                
                metadata <- data.frame(
                    comparison = c(names(plot_list)),
                    data_source = c(
                        rep("d1", length(input$upset_ref_comparisons) * 2),
                        rep("d2", length(input$upset_comp_comparisons) * 2)
                    )
                )
            }
            
            upset_metadata_obj <- list(
                data = metadata,
                plots = list(list(
                    type = "matrix_rows", 
                    column = "data_source",
                    colors = c(d1 = "navy", d2 = "red"),
                    alpha=0.2
                ))
            )
        }
        else {
            if (!input$fold_split_upset) {
                plot_list <- ref_names_list
                names(plot_list) <- input$upset_ref_comparisons
            }
            else {
                plot_list_joint <- ref_names_list
                names(plot_list_joint) <- input$upset_ref_comparisons %>% gsub("\\.$", "", .)
                plot_list <- lapply(rapply(plot_list_joint, enquote, how="unlist"), eval)
            }
            upset_metadata_obj <- NULL
        }
        
        plt <- UpSetR::upset(
            UpSetR::fromList(plot_list), 
            set.metadata = upset_metadata_obj,
            order.by="freq", 
            text.scale=2, 
            nsets = input$upset_max_comps
        )
        plt
    }, height = 800)
    
    output$fold_comp <- renderPlot({
        
        ref_names_list <- lapply(input$upset_ref_comparisons, function(stat_pattern, dataset, contrast_type) {
            parse_contrast_pass_list(dataset, stat_pattern, contrast_type) %>% names()
        }, dataset=input$dataset1, contrast_type=input$contrast_type)
        
        if (input$dataset1 != input$dataset2) {
            comp_names_list <- lapply(input$upset_comp_comparisons, function(stat_pattern, dataset, contrast_type) {
                parse_contrast_pass_list(dataset, stat_pattern, contrast_type) %>% names()
            }, dataset=input$dataset2, contrast_type=input$contrast_type)
            
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
        
        if (input$dataset1 == input$dataset2) {
            long_df <- rv$mapping_obj()$get_combined_dataset() %>% 
                filter(comb_id %in% present_in_all) %>%
                mutate(
                    p_sum=rowSums(.[, paste0("d1.", input$upset_comp_comparisons, "P.Value"), drop=FALSE]),
                    p_prod=rowSums(.[, paste0("d1.", input$upset_comp_comparisons, "P.Value"), drop=FALSE])
                ) %>%
                arrange(p_sum) %>%
                head(input$max_fold_comps) %>%
                dplyr::select(ID=comb_id, p_sum=p_sum, paste0("d1.", input$upset_ref_comparisons, "logFC")
                ) %>%
                tidyr::gather("Comparison", "Fold", -ID, -p_sum)
            
        }
        else {
            long_df <- rv$mapping_obj()$get_combined_dataset() %>% 
                filter(comb_id %in% present_in_all) %>%
                mutate(
                    p_sum=rowSums(.[, 
                                    c(paste0("d1.", input$upset_ref_comparisons, "P.Value"),
                                      paste0("d2.", input$upset_comp_comparisons, "P.Value")),
                                    drop=FALSE]),
                    p_prod=rowSums(.[, 
                                     c(paste0("d1.", input$upset_ref_comparisons, "P.Value"),
                                       paste0("d2.", input$upset_comp_comparisons, "P.Value")),
                                     drop=FALSE])
                ) %>%
                arrange(p_sum) %>%
                head(input$max_fold_comps) %>%
                dplyr::select(ID=comb_id, 
                              p_sum=p_sum,
                              paste0("d1.", input$upset_ref_comparisons, "logFC"),
                              paste0("d2.", input$upset_comp_comparisons, "logFC")
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
        combined_dataset <- rv$mapping_obj()$get_combined_dataset(full_entries=FALSE)
        
        plot_df <- data.frame(
            ref_sig = combined_dataset[[rv$statcols_ref(rv, input$dataset1, input$ref_contrast)$P.Value]],
            ref_fold = combined_dataset[[rv$statcols_ref(rv, input$dataset1, input$ref_contrast)$logFC]],
            comp_sig = combined_dataset[[rv$statcols_ref(rv, input$dataset2, input$comp_contrast)$P.Value]],
            comp_fold = combined_dataset[[rv$statcols_ref(rv, input$dataset2, input$comp_contrast)$logFC]]
        ) %>% 
            mutate(highest_p=pmax(ref_sig, comp_sig)) %>% 
            arrange(highest_p) %>%
            mutate(is_contra=sign(ref_fold) != sign(comp_fold)) %>%
            mutate(tot_contra=cumsum(is_contra), tot_same=cumsum(!is_contra)) %>%
            mutate(cum_frac_contra=tot_same/(tot_same+tot_contra))
        
        # area <- plot_df %>% group_by(tot_contra) %>% group_map(~min(.$tot_same_frac)) %>% unlist() %>% sum()
        
        plt_full <- ggplot() + 
            geom_line(data=data.frame(x=c(0, plot_df$tot_contra %>% max()), y=c(0, plot_df$tot_same %>% max())), aes(x=x, y=y), size=1) +
            geom_line(data=plot_df, aes(x=tot_contra, y=tot_same, color=highest_p<input$threshold), size=2) +
            ggtitle("Number same-fold features, incorrect area calc: %s, p-value colored threshold")
        
        plt_subset <- ggplot() + 
            geom_line(data=plot_df %>% filter(highest_p < input$threshold), aes(x=tot_contra, y=tot_same), size=2) +
            ggtitle("Zoomed in below threshold")
        
        plt_cumfrac_over_logp <- ggplot(plot_df, aes(x=log10(highest_p), y=cum_frac_contra)) + geom_line()
        plt_cumfrac_over_p <- ggplot(plot_df, aes(x=highest_p, y=cum_frac_contra)) + geom_line()
        
        ggarrange(plt_full, plt_subset, plt_cumfrac_over_logp, plt_cumfrac_over_p, ncol=2, nrow=2)
    })
    
    output$table_display <- DT::renderDataTable({
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
    
    sync_param_choices <- function() {
        warning("Empty overlap parameter sync for now, probably needed for updating datasets!")
        # ref_choices <- c("None", rv$ddf_cols_ref(rv, input$dataset1))
        # comp_choices <- c("None", rv$ddf_cols_comp(rv, input$dataset2))
        # # updateSelectInput(session, "color_data_ref", choices = ref_choices, selected=ref_choices[1])
        # # updateSelectInput(session, "sample_data1", choices = ref_choices, selected=ref_choices[1])
        # # updateSelectInput(session, "color_data_comp", choices = comp_choices, selected=comp_choices[1])
        # # updateSelectInput(session, "sample_data2", choices = comp_choices, selected=comp_choices[1])
        # 
        # ref_data_choices <- c("None", rv$rdf_cols_ref(rv, input$dataset1))
        # comp_data_choices <- c("None", rv$rdf_cols_comp(rv, input$dataset2))
        # updateSelectInput(session, "data_num_col_ref", choices = ref_data_choices, selected=ref_data_choices[1])
        # updateSelectInput(session, "data_cat_col_ref", choices = ref_data_choices, selected=ref_data_choices[1])
        # updateSelectInput(session, "data_num_col_comp", choices = comp_data_choices, selected=comp_data_choices[1])
        # updateSelectInput(session, "data_cat_col_comp", choices = comp_data_choices, selected=comp_data_choices[1])
    }
    
    observeEvent(rv$ddf_ref(rv, input$dataset1), {
        sync_param_choices()
    })
    
    observeEvent(rv$ddf_comp(rv, input$dataset2), {
        sync_param_choices()
    })
    
    
    observeEvent({
        rv$selected_cols_obj() 
        input$dataset1 
        input$dataset2}, {
            if (is.null(rv$filename_1()) && is.null(rv$filename_2())) {
                return()
            }
            
            choices_1 <- rv$selected_cols_obj()[[input$dataset1]]$statpatterns
            choices_2 <- rv$selected_cols_obj()[[input$dataset2]]$statpatterns
            
            updateSelectInput(session, "ref_contrast", choices=choices_1, selected=choices_1[1])
            updateSelectInput(session, "comp_contrast", choices=choices_2, selected=choices_2[1])
            updateSelectInput(session, "upset_ref_comparisons", choices=choices_1, selected=choices_1)
            updateSelectInput(session, "upset_comp_comparisons", choices=choices_2, selected=choices_2)
        })
    
    output$warnings <- renderUI({
        
        error_vect <- c()
        if (is.null(rv$filename_1())) {
            error_vect <- c(error_vect, "No filename_1 found, upload dataset at Setup page")
        }
        
        if (is.null(rv$design_1())) {
            error_vect <- c(error_vect, "No design_1 found, upload dataset at Setup page")
        }
        
        total_text <- paste(error_vect, collapse="<br>")
        HTML(sprintf("<b><font size='5' color='red'>%s</font></b>", total_text))
    })
}

