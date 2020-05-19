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
                        conditionalPanel(
                            sprintf("input['%s'] != 'UpsetPresence'", ns("plot_tabs")),
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
                            )
                        ),
                        conditionalPanel(
                            sprintf("input['%s'] == 'UpsetPresence'", ns("plot_tabs")),
                            fluidRow(
                                column(6, selectInput(ns("upset_pres_cond_ref"), "Condition ref.", choices=c(""), selected="")),
                                column(6, selectInput(ns("upset_pres_levels_ref"), "Selected levels ref.", choices=c(""), selected="", multiple=TRUE))
                            ),
                            conditionalPanel(
                                sprintf("input['%s'] != input['%s']", ns("dataset1"), ns("dataset2")),
                                fluidRow(
                                    column(6, selectInput(ns("upset_pres_cond_comp"), "Condition comp.", choices=c(""), selected="")),
                                    column(6, selectInput(ns("upset_pres_levels_comp"), "Selected levels comp.", choices=c(""), selected="", multiple=TRUE))
                                )
                            ),
                            fluidRow(column(12, sliderInput(ns("upset_pres_frac"), "Required minimum fraction to be considered present", value=0, min=0, max=1, step=0.05)))
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
                            sprintf("input['%s'] == 'Upset' || input['%s'] == 'UpsetPresence'", ns("plot_tabs"), ns("plot_tabs")),
                            fluidRow(
                                column(6, numericInput(ns("upset_max_comps"), "Upset max comparisons (rows)", min = 1, value = 10)),
                                column(6, numericInput(ns("upset_max_intersects"), "Upset max intersects (columns)", min = 1, value = 40))
                            ),
                            fluidRow(
                                column(6, checkboxInput(ns("upset_degree_order"), "Order on degree", value=FALSE)),
                                conditionalPanel(
                                    sprintf("input['%s'] == 'Upset'", ns("plot_tabs")),
                                    column(6, checkboxInput(ns("fold_split_upset"), "Fold split upset", value=FALSE))
                                )
                            )
                        ),
                        conditionalPanel(
                            sprintf("input['%s'] == 'FoldComparison'", ns("plot_tabs")),
                            numericInput(ns("max_fold_comps"), "Max fold comps", min=1, value=10)
                        )
                    ),
                    # htmlOutput(ns("warnings")),
                    conditionalPanel(
                        sprintf("input['%s'] == 'Upset'", ns("plot_tabs")),
                        wellPanel(
                            h3("Select overlap"),
                            selectInput(ns("upset_crosssec_display"), "Display cross-section", choices = c("Dev"), selected="Dev", multiple = TRUE),
                            textOutput(ns("test_crosssec_display"))
                        )
                    ),
                    conditionalPanel(
                        sprintf("input['%s'] == 'UpsetPresence'", ns("plot_tabs")),
                        wellPanel(
                            h3("Select overlap"),
                            selectInput(ns("upset_crosssec_display_presence"), "Display cross-section (presence)", choices = c("Dev"), selected="Dev", multiple = TRUE)
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
                                 plotOutput(ns("upset"), height = 800) %>% withSpinner(),
                                 downloadButton(ns("download_table_upset"), "Download table"),
                                 DT::DTOutput(ns("table_display_upset"))
                        ),
                        tabPanel("FoldComparison",
                                 plotOutput(ns("fold_comp"))
                        ),
                        tabPanel("UpsetPresence",
                                 plotOutput(ns("upset_presence"), height = 800) %>% withSpinner(),
                                 downloadButton(ns("download_table_upset_presence"), "Download table"),
                                 DT::DTOutput(ns("table_display_upset_presence"))
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
    
    output$download_table <- output$download_table_upset <- downloadHandler(
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
    
    parsed_presence_entries <- reactive({
        if (input$dataset1 == input$dataset2) {
            input$upset_pres_levels_ref
        }
        else {
            c(paste("d1", input$upset_pres_levels_ref, sep="."),
              paste("d2", input$upset_pres_levels_comp, sep="."))
        }
    })
    
    observeEvent(parsed_overlap_entries(), {
        updateSelectInput(session, "upset_crosssec_display", choices=parsed_overlap_entries(), selected = parsed_overlap_entries())
    })
    
    observeEvent(parsed_presence_entries(), {
        updateSelectInput(session, "upset_crosssec_display_presence", choices=parsed_presence_entries(), selected = parsed_presence_entries())
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
    
    output_table_reactive <- reactive({
        
        validate(need(!is.null(rv$mapping_obj()), "No mapping object found, are samples mapped at the Setup page?"))
        validate(need(!is.null(rv$mapping_obj()$get_combined_dataset()), "No combined dataset found, are samples mapped at the Setup page?"))
        
        get_target_ids_from_presence <- function(presence_df, all_conditions, selected_conditions) {
            non_selected_conditions <- all_conditions %>% discard(~. %in% selected_conditions)
            presence_inintersect_df <- presence_df %>% 
                filter_at(vars(all_of(selected_conditions)), ~.==1)
            
            if (length(non_selected_conditions) > 0) {
                presence_inintersect_notinothers_df <- presence_inintersect_df %>% 
                    filter_at(vars(all_of(non_selected_conditions)), ~.==0)
                target_ids <- presence_inintersect_notinothers_df %>% pull(id_col)
            }
            else {
                target_ids <- presence_inintersect_df %>% pull(id_col)
            }
            target_ids
        }
        
        
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

            get_is_element <- function(target_feature, all_features){
                is.element(all_features, target_feature)
            }
            
            contrast_features_list <- upset_plot_list()
            all_features <- contrast_features_list %>% unlist() %>% unique()

            presence_df <- lapply(contrast_features_list, get_is_element, all_features=all_features) %>% 
                map(as.integer) %>% 
                data.frame() %>%
                mutate(id_col=all_features)
                
            all_contrasts <- UpSetR::fromList(contrast_features_list) %>% colnames()
            target_ids <- get_target_ids_from_presence(presence_df, all_contrasts, input$upset_crosssec_display)
            
            # presence_inintersect_df <- lapply(contrast_features_list, get_is_element, all_features=all_features) %>% 
            #     map(as.integer) %>% 
            #     data.frame() %>%
            #     mutate(id_col=all_features) %>%
            #     filter_at(vars(all_of(input$upset_crosssec_display)), ~.==1)

            # non_selected_contrasts <- all_contrasts %>% discard(~ . %in% input$upset_crosssec_display)
            # if (length(non_selected_contrasts) > 0) {
            #     presence_inintersect_notinothers_df <- presence_inintersect_df %>% filter_at(vars(all_of(non_selected_contrasts)), ~.==0)
            #     output_df <- presence_inintersect_notinothers_df
            # }
            # else {
            #     output_df <- presence_inintersect_df
            # }
            # target_ids <- output_df %>% pull(id_col)
        }
        else if (input$plot_tabs == "UpsetPresence") {
            
            presence_df <- upset_presence_dataframe() %>% 
                filter_at(vars(!matches("^comb_id$")), any_vars(. != "0")) %>%
                dplyr::rename(id_col=comb_id)
            all_condition_levels <- UpSetR::fromList(presence_df) %>% colnames() %>% discard(~.=="id_col")
            target_ids <- get_target_ids_from_presence(presence_df, all_condition_levels, input$upset_crosssec_display_presence)
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
    
    upset_presence_dataframe <- reactive({
        nbr_nas <- get_na_nbrs_uppres(rv, input, input$upset_pres_cond_ref, input$upset_pres_levels_ref, dataset_nbr=1, target="ref")
        upset_table <- parse_na_nbrs_to_upset_table(
            nbr_nas, 
            input$dataset1, 
            rv$ddf_ref(rv, input$dataset1), 
            input$upset_pres_cond_ref,
            input$upset_pres_levels_ref,
            presence_fraction_thres=input$upset_pres_frac)

        if (input$dataset1 != input$dataset2) {
            nbr_nas_comp <- get_na_nbrs_uppres(rv, input, input$upset_pres_cond_comp, input$upset_pres_levels_comp, dataset_nbr=2, target="comp")
            upset_table_comp <- parse_na_nbrs_to_upset_table(
                nbr_nas_comp, 
                input$dataset2, 
                rv$ddf_comp(rv, input$dataset2), 
                input$upset_pres_cond_comp,
                input$upset_pres_levels_comp,
                presence_fraction_thres=input$upset_pres_frac)
            
            ref_count <- ncol(upset_table) - 1
            comp_count <- ncol(upset_table_comp) - 1
            upset_table <- cbind(
                upset_table[, -ncol(upset_table)] %>% rename_all(~paste("d1", ., sep=".")),
                upset_table_comp %>% rename_at(vars(!matches("^comb_id$")), ~paste("d2", ., sep="."))
            )
        }
        upset_table
    })
    
    # upset_plot_list_comp <- reactive({
    #     validate(need(input$dataset1 != input$dataset2, "Should only be called with different dataset1 and dataset2 selected"))
    #     comp_names_list <- upset_extract_set_names_list(rv, input, input$upset_comp_comparisons, input$dataset2, input$stat_contrast_type, input$fold_split_upset)
    #     plot_list_comp <- upset_get_plot_list(comp_names_list, input$upset_comp_comparisons, input$fold_split_upset)
    #     plot_list_comp
    # })
    
    upset_name_order <- reactive({
        plot_list <- upset_plot_list()
        upset_get_name_order(plot_list, input$fold_split_upset)
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
        upset_metadata_obj
    })
    
    upset_order_by <- reactive({
        if (input$upset_degree_order) {
            "degree"
        }
        else {
            "freq"
        }
    })
    
    output$upset_presence <- renderPlot({

        upset_table <- upset_presence_dataframe()
        metadata <- NULL
        
        if (input$dataset1 != input$dataset2) {
            metadata <- list(
                data = data.frame(
                    source=colnames(upset_table)[-ncol(upset_table)],
                    dataset=c(
                        rep("d1", ref_count),
                        rep("d2", comp_count)
                    )
                ),
                plots = list(list(
                    type = "matrix_rows",
                    column = "dataset",
                    colors = c(d1="navy", d2="red"),
                    alpha=0.2
                ))
            )
        }
        
        set_ordering <- get_ordered_sets(upset_table[, -ncol(upset_table)], order_on = upset_order_by())
        name_order <- upset_table[, -ncol(upset_table)] %>% colnames()
        bar_coloring <- (set_ordering$string_entries == paste(input$upset_crosssec_display_presence, collapse=",")) %>% ifelse("#298ff5", "gray23")
        
        UpSetR::upset(
            upset_table[, -ncol(upset_table)], 
            set.metadata=metadata,
            order.by=upset_order_by(), 
            sets=name_order,
            keep.order=TRUE,
            text.scale=2, 
            nsets=input$upset_max_comps,
            nintersects=input$upset_max_intersects,
            main.bar.color=bar_coloring
        )
    })
    
    output$upset <- renderPlot({
        
        plot_list <- upset_plot_list()
        name_order <- upset_name_order()
        upset_metadata_obj <- upset_metadata()
        
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
    
    output$table_display <- output$table_display_upset <- output$table_display_upset_presence <- DT::renderDataTable({
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
    
    observeEvent(input$upset_pres_cond_ref, {
        req(rv$ddf_ref(rv, input$dataset1))
        choices <- rv$ddf_ref(rv, input$dataset1)[[input$upset_pres_cond_ref]] %>% unique() 
        updateSelectInput(session, "upset_pres_levels_ref", choices=choices, selected=choices)
    })
    
    observeEvent(input$upset_pres_cond_comp, {
        req(rv$ddf_comp(rv, input$dataset2))
        choices <- rv$ddf_comp(rv, input$dataset2)[[input$upset_pres_cond_comp]] %>% unique() 
        updateSelectInput(session, "upset_pres_levels_comp", choices=choices, selected=choices)
    })
    
    observeEvent({
        rv$selected_cols_obj() 
        input$dataset1 
        input$dataset2}, {
            req(rv$filename_1())
            req(rv$ddf_ref(rv, input$dataset1))
            req(rv$ddf_comp(rv, input$dataset2))
            
            # set_if_new <- function(prev_val, new_values, new_val_selected) {
            #     if (is.null(prev_val)) new_val_selected
            #     else if (prev_val %in% new_values) prev_val
            #     else new_val_selected
            # }
            
            choices_1 <- rv$selected_cols_obj()[[input$dataset1]]$statpatterns
            updateSelectInput(session, "ref_contrast", choices=choices_1, selected=choices_1[1])
            updateSelectInput(session, "upset_ref_comparisons", choices=choices_1, selected=choices_1)
            
            choices_2 <- rv$selected_cols_obj()[[input$dataset2]]$statpatterns
            updateSelectInput(session, "comp_contrast", choices=choices_2, selected=choices_2[1])
            updateSelectInput(session, "upset_comp_comparisons", choices=choices_2, selected=choices_2)
            
            ref_cond_choices <- c("None", rv$ddf_cols_ref(rv, input$dataset1))
            updateSelectInput(session, "upset_pres_cond_ref", choices = ref_cond_choices, selected=ref_cond_choices[2])
            
            comp_cond_choices <- c("None", rv$ddf_cols_comp(rv, input$dataset2))
            updateSelectInput(session, "upset_pres_cond_comp", choices = comp_cond_choices, selected=comp_cond_choices[2])
        })
}

