setup_reactive_values_obj <- function(input) {
    
    get_filename <- function(in_file) {
        infile <- in_file
        if (is.null(infile)) {
            return(NULL)
        }
        stringi::stri_extract_first(str = infile$name, regex = ".*")
    }
    
    load_data <- function(in_file) {
        infile <- in_file
        if (is.null(infile)) {
            return(NULL)
        }
        raw_df <- read_tsv(infile$datapath, col_types = cols())
        colnames(raw_df) <- make.names(colnames(raw_df))
        raw_df
    }
    
    rv <- list()
    rv$filedata_1 <- reactive(load_data(input$data_file_1))
    rv$filedata_2 <- reactive(load_data(input$data_file_2))
    rv$design_1 <- reactive(load_data(input$design_file_1))
    rv$design_2 <- reactive({
        if (!input$matched_samples) {
            load_data(input$design_file_2)
        }
        else {
            load_data(input$design_file_1)
        }
    })
    rv$design_samplecol_1 <- reactive(input$design_sample_col_1)
    rv$design_samplecol_2 <- reactive(input$design_sample_col_2)
    rv$design_condcol_1 <- reactive(input$design_cond_col_1)
    rv$design_condcol_2 <- reactive(input$design_cond_col_2)
    rv$selected_cols_obj <- reactiveVal(list())
    rv$filename_1 <- reactive(get_filename(input$data_file_1))
    rv$filename_2 <- reactive(get_filename(input$data_file_2))
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
    
    rv$rdf_ref <- function(rv, input_field) retrieve_data(rv, input_field, 1, "filedata")
    rv$rdf_comp <- function(rv, input_field) retrieve_data(rv, input_field, 2, "filedata")
    rv$ddf_ref <- function(rv, input_field) retrieve_data(rv, input_field, 1, "design")
    rv$ddf_comp <- function(rv, input_field) retrieve_data(rv, input_field, 2, "design")
    rv$rdf_cols_ref <- function(rv, input_field) colnames(retrieve_data(rv, input_field, 1, "filedata"))
    rv$rdf_cols_comp <- function(rv, input_field) colnames(retrieve_data(rv, input_field, 2, "filedata"))
    rv$ddf_cols_ref <- function(rv, input_field) colnames(retrieve_data(rv, input_field, 1, "design"))
    rv$ddf_cols_comp <- function(rv, input_field) colnames(retrieve_data(rv, input_field, 2, "design"))
    
    rv$table_settings <- reactiveVal(NULL)
    
    rv$ddf_condcol_ref <- function(rv, input_field) { 
        # browser()
        req(input_field != "")
        rv[[sprintf("design_condcol_%s", di_new(rv, input_field, 1))]]()
    } 
    rv$ddf_condcol_comp <- function(rv, input_field) { 
        req(input_field != "")
        rv[[sprintf("design_condcol_%s", di_new(rv, input_field, 2))]]() 
    } 
    
    rv$ddf_samplecol_ref <- function(rv, input_field) {
        rv[[sprintf("design_samplecol_%s", di_new(rv, input_field, 1))]]()
    }
    rv$ddf_samplecol_comp <- function(rv, input_field) {
        rv[[sprintf("design_samplecol_%s", di_new(rv, input_field, 2))]]()
    }
    
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
    
    rv$dt_parsed_data <- function(rv, shown_data, with_row_selection=TRUE) {
        
        table_settings <- rv$table_settings()
        # shown_data <- rv$mapping_obj()$get_combined_dataset()
        
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

        page_length <- 10
        display_pos <- selected_row_nbr - (selected_row_nbr %% page_length)
                
        parsed_shown_data <- shown_data %>%
            mutate_if(
                is.character,
                ~str_trunc(., table_settings$trunc_length)
            ) %>%
            mutate_if(
                is.numeric,
                ~round(., table_settings$round_digits)
            ) 
        
        if (with_row_selection) {
            parsed_shown_data %>% 
                dplyr::select(table_settings$shown_fields) %>%
                    DT::datatable(data=., 
                    selection=list(mode='single', selected=c(selected_row_nbr)),
                    options=list(pageLength=page_length, displayStart=display_pos))
        }
        else {
            parsed_shown_data
        }
        

    }
    
    rv
}