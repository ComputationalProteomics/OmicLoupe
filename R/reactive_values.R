setup_reactive_values_obj <- function(input, preloaded = NULL) {

	    get_preloaded <- function() {
	        if (is.null(preloaded)) {
	            return(get_preloaded_data())
	        }
	        if (is.function(preloaded)) {
	            return(preloaded())
	        }
	        preloaded
	    }

	    get_filename <- function(in_file, preloaded_name = NULL) {
	        if (!is.null(preloaded_name)) {
	            return(preloaded_name)
        }

        infile <- in_file
        if (is.null(infile)) {
            return(NULL)
        }
        stringi::stri_extract_first(str = infile$name, regex = ".*")
    }

    load_data <- function(in_file, two_datasets=NULL, is_design_file = FALSE, preloaded_data = NULL) {
        if (!is.null(preloaded_data)) {
            return(preloaded_data)
        }

        infile <- in_file
        if (is.null(infile) || (is.logical(two_datasets) && two_datasets==FALSE)) {
            return(NULL)
        }
        raw_df <- read_tsv(infile$datapath, col_types = cols())

        raw_df
    }

    rv <- list()
	    rv$setup_input <- reactive(input)

		    rv$filedata_1 <- reactive({
		        if (!is.null(input$data_file_1)) {
		            return(load_data(input$data_file_1))
		        }
		        preloaded_now <- get_preloaded()
		        if (!is.null(preloaded_now) && !is.null(preloaded_now$data1)) {
		            return(preloaded_now$data1)
		        }
		        NULL
		    })

		    rv$filedata_2 <- reactive({
		        if (!is.null(input$data_file_2)) {
		            return(load_data(input$data_file_2, input$two_datasets))
		        }
		        preloaded_now <- get_preloaded()
		        if (!is.null(preloaded_now) && !is.null(preloaded_now$data2)) {
		            return(preloaded_now$data2)
		        }
		        NULL
		    })

		    rv$design_1 <- reactive({
		        if (!is.null(input$design_file_1)) {
		            return(load_data(input$design_file_1, is_design_file = TRUE))
		        }
		        preloaded_now <- get_preloaded()
		        if (!is.null(preloaded_now) && !is.null(preloaded_now$design1)) {
		            return(preloaded_now$design1)
		        }
		        NULL
		    })

		    rv$design_2 <- reactive({
		        preloaded_now <- get_preloaded()
		        matched <- if (!is.null(preloaded_now) && !is.null(preloaded_now$matched_samples)) {
		            isTRUE(preloaded_now$matched_samples)
		        } else {
		            isTRUE(input$matched_samples)
		        }

		        if (matched) {
		            if (!is.null(input$design_file_1)) {
		                return(load_data(input$design_file_1, is_design_file = TRUE))
		            }
		            if (!is.null(preloaded_now) && !is.null(preloaded_now$design1)) {
		                return(preloaded_now$design1)
		            }
		            return(load_data(input$design_file_1, is_design_file = TRUE))
		        }

		        if (!is.null(input$design_file_2)) {
		            return(load_data(input$design_file_2, is_design_file = TRUE))
		        }
		        if (!is.null(preloaded_now) && !is.null(preloaded_now$design2)) {
		            return(preloaded_now$design2)
		        }
		        load_data(input$design_file_2, is_design_file = TRUE)
		    })
    rv$design_samplecol_1 <- reactive(input$design_sample_col_1)
    rv$design_samplecol_2 <- reactive(input$design_sample_col_2)
    rv$design_condcol_1 <- reactive(input$design_cond_col_1)
    rv$design_condcol_2 <- reactive(input$design_cond_col_2)
    rv$data_featurecol_1 <- reactive(input$feature_col_1)
    rv$data_featurecol_2 <- reactive(input$feature_col_2)
    rv$data_annotcol_1 <- reactive(input$annot_col_1)
    rv$data_annotcol_2 <- reactive(input$annot_col_2)

    rv$figure_save_format <- reactive(input$figure_save_format)
    rv$figure_save_width <- reactive(input$figure_save_width)
    rv$figure_save_height <- reactive(input$figure_save_height)
    rv$figure_save_dpi <- reactive(input$figure_save_dpi)

		    rv$selected_cols_obj <- reactiveVal(list())
		    rv$filename_1 <- reactive({
		        if (!is.null(input$data_file_1)) {
		            return(get_filename(input$data_file_1))
		        }
		        preloaded_now <- get_preloaded()
		        if (!is.null(preloaded_now) && !is.null(preloaded_now$data1)) {
		            preloaded_name <- if (isTRUE(identical(preloaded_now$handoff_source, "NormalyzerDE"))) {
		                "NormalyzerDE"
		            } else {
		                "PreloadedData1"
		            }
		            return(get_filename(NULL, preloaded_name = preloaded_name))
		        }
		        NULL
		    })
		    rv$filename_2 <- reactive({
		        if (!is.null(input$data_file_2)) {
		            return(get_filename(input$data_file_2))
		        }
		        preloaded_now <- get_preloaded()
		        if (!is.null(preloaded_now) && !is.null(preloaded_now$data2)) {
		            return(get_filename(NULL, preloaded_name = "PreloadedData2"))
		        }
		        NULL
	    })
    rv$mapping_obj <- reactiveVal(NULL)
    rv$selected_feature <- reactiveVal(NULL)
    rv$selected_feature_module <- reactiveVal(NULL)

    rv$set_selected_feature <- function(feature, module_name) {
        rv$selected_feature(feature)
        rv$selected_feature_module(module_name)
    }

    rv$correlations <- reactiveVal(NULL)

    retrieve_data <- function(rv, input_field, ind, data_pat) {
        if (!is.null(di_new(rv, input_field, 1))) rv[[sprintf("%s_%s", data_pat, di_new(rv, input_field, ind))]]()
        else NULL
    }

    rv$rdf_ref <-               function(rv, input_field) retrieve_data(rv, input_field, 1, "filedata")
    rv$rdf_comp <-              function(rv, input_field) retrieve_data(rv, input_field, 2, "filedata")
    rv$ddf_ref <-               function(rv, input_field) retrieve_data(rv, input_field, 1, "design")
    rv$ddf_comp <-              function(rv, input_field) retrieve_data(rv, input_field, 2, "design")
    rv$rdf_cols_ref <-          function(rv, input_field) colnames(retrieve_data(rv, input_field, 1, "filedata"))
    rv$rdf_cols_comp <-         function(rv, input_field) colnames(retrieve_data(rv, input_field, 2, "filedata"))
    rv$ddf_cols_ref <-          function(rv, input_field) colnames(retrieve_data(rv, input_field, 1, "design"))
    rv$ddf_cols_comp <-         function(rv, input_field) colnames(retrieve_data(rv, input_field, 2, "design"))

    rv$table_settings <- reactiveVal(NULL)
    rv$ddf_condcol_ref <- function(rv, input_field) {
        shiny::validate(need(input_field != "", "No condition column found for reference data"))
        rv[[sprintf("design_condcol_%s", di_new(rv, input_field, 1))]]()
    }
    rv$ddf_condcol_comp <- function(rv, input_field) {
        shiny::validate(need(input_field != "", "No condition column found for comparison data"))
        rv[[sprintf("design_condcol_%s", di_new(rv, input_field, 2))]]()
    }

    rv$ddf_samplecol_ref <- function(rv, input_field) {
        sample_col_string <- sprintf("design_samplecol_%s", di_new(rv, input_field, 1))
        shiny::validate(need(
            !is.null(di_new(rv, input_field, 1)) && sample_col_string %in% names(rv),
            "Reference samples not found, have you loaded a dataset and mapped sample columns?"))
        rv[[sample_col_string]]()
    }
    rv$ddf_samplecol_comp <- function(rv, input_field) {
        sample_col_string <- sprintf("design_samplecol_%s", di_new(rv, input_field, 2))
        shiny::validate(need(
            !is.null(di_new(rv, input_field, 1)) && sample_col_string %in% names(rv),
            "Comparison samples not found, have you loaded a dataset and mapped sample columns?"))
        rv[[sample_col_string]]()
    }

    rv$rdf_featurecol_ref <- function(rv, input_field)
        rv[[sprintf("data_featurecol_%s", di_new(rv, input_field, 1))]]()
    rv$rdf_featurecol_comp <- function(rv, input_field)
        rv[[sprintf("data_featurecol_%s", di_new(rv, input_field, 2))]]()
    rv$rdf_annotcol_ref <- function(rv, input_field)
        rv[[sprintf("data_annotcol_%s", di_new(rv, input_field, 1))]]()
    rv$rdf_annotcol_comp <- function(rv, input_field)
        rv[[sprintf("data_annotcol_%s", di_new(rv, input_field, 2))]]()

    rv$statsuffixes <- function(rv, input_field)
        rv$selected_cols_obj()[[input_field]]$statpatterns

    rv$samples <- function(rv, input_field, prefix="") {
        paste(prefix, rv$selected_cols_obj()[[input_field]]$samples, sep="")
    }

    rv$calculate_preselect_index <- function(rv, shown_data) {
        if (is.null(rv$selected_feature())) {
            1
        }
        else {
            which(shown_data$comb_id %>% as.character() %in% rv$selected_feature())
        }
    }

    rv$dt_parsed_data_raw <- function(rv, shown_data) {
        table_settings <- rv$table_settings()

        parsed_shown_data <- shown_data %>%
            mutate(
                across(where(is.character), ~str_trunc(., table_settings$trunc_length)),
                across(where(is.numeric), ~round(., table_settings$round_digits))
            )
        parsed_shown_data
    }

    rv$dt_parsed_data <- function(rv, shown_data, with_row_selection=TRUE, add_show_cols_first=NULL, add_show_cols_last=NULL, selection_mode='single') {

        table_settings <- rv$table_settings()
        parsed_shown_data <- rv$dt_parsed_data_raw(rv, shown_data)

        if (is.null(rv$selected_feature())) {
            selected_row_nbr <- 1
        }
        else {
            target_index <- which(shown_data$comb_id %>% as.character() %in% rv$selected_feature())
            if (length(target_index) == 0) {
                selected_row_nbr <- 1
            }
            else {
                selected_row_nbr <- target_index
            }
        }

        page_length <- DEFAULT_PAGE_LENGTH
        display_pos <- (selected_row_nbr-1) - ((selected_row_nbr-1) %% page_length)

        if (with_row_selection) {
            parsed_shown_data %>%
                dplyr::select(all_of(c(add_show_cols_first, table_settings$shown_fields, add_show_cols_last))) %>%
                DT::datatable(data=.,
                              selection=list(mode=selection_mode, selected=c(selected_row_nbr)),
                              options=list(
                                  pageLength=page_length,
                                  displayStart=display_pos
                              ))
        }
        else {
            parsed_shown_data
        }
    }

    rv
}
