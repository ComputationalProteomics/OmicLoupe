get_ordered_sets <- function(upset_list, order_on, name_order, omit_empty=TRUE) {
    
    # Translates "1010" to retrieving contrast names 1 and 3 in a list
    retrieve_contrasts_from_union_string <- function(union_string_list, name_order) {
        str_split(union_string_list, "") %>% map(~ifelse(.=="1", T, F) %>% name_order[.])
        # str_split(union_string_list, "") %>% map(~ifelse(.=="1", T, F) %>% names(upset_list)[.])
    }

    ordered_upset_list <- upset_list[name_order]
    
    # name_order
    
    unordered <- ordered_upset_list %>% 
        unite("union_contrast_string", names(ordered_upset_list), sep="", remove = FALSE) %>% 
        group_by(union_contrast_string) %>% 
        summarize(nbr=n()) %>% 
        dplyr::mutate(grade=union_contrast_string %>% gsub("0", "", .) %>% str_length()) %>%
        dplyr::mutate(included_entries=retrieve_contrasts_from_union_string(union_contrast_string, name_order)) %>%
        dplyr::mutate(string_entries=map(included_entries, ~paste(., collapse=",")) %>% unlist())
    
    # Generate name order here?
    #name_order
    
    if (omit_empty && unordered %>% dplyr::filter(grade != 0) %>% nrow() > 0) {
        message(sprintf("Omitting %s entries of grade 0", unordered %>% dplyr::filter(grade != 0) %>% nrow()))
        unordered <- unordered %>% dplyr::filter(grade != 0)
    }
    
    if (order_on == "freq") {
        unordered %>% arrange(desc(nbr), union_contrast_string)
    }
    else if (order_on == "degree") {
        unordered %>% arrange(desc(grade), union_contrast_string)
    }
    else {
        stop("Unknown ordering condition: ", order_on)
    }
}

parse_contrast_pass_list <- function(rv, input, target_data, target_contrast, contrast_type) {
    
    validate(need(!is.null(rv$mapping_obj()), "No loaded data found, is everything set up at the Setup page?"))
    
    combined_dataset <- rv$mapping_obj()$get_combined_dataset(full_entries=FALSE)
    sig_field <- rv$statcols_ref(rv, target_data, target_contrast)[[contrast_type]]
    fold_field <- rv$statcols_ref(rv, target_data, target_contrast)$logFC
    
    pass_stat_contrast <- combined_dataset[, sig_field] < input$stat_threshold
    
    if (input$use_fold_cutoff) {
        pass_fold_contrast <- abs(combined_dataset[, fold_field]) > input$fold_threshold
    }
    else {
        pass_fold_contrast <- TRUE
    }
    
    pass_all_contrast <- pass_stat_contrast & pass_fold_contrast
    
    pass_tbl <- combined_dataset %>%
        dplyr::filter(pass_all_contrast) %>%
        dplyr::select(all_of(c("comb_id", fold_field))) %>%
        dplyr::rename(fold=fold_field) %>%
        mutate(comb_id=as.character(comb_id))
    
    pass_list <- setNames(as.list(pass_tbl$fold), pass_tbl$comb_id)
    pass_list
}

upset_extract_set_names_list <- function(rv, input, comparisons, dataset, contrast_type, fold_split) {
    lapply(comparisons, function(stat_pattern, dataset, contrast_type, fold_split) {
        joint_entries_w_fold <- parse_contrast_pass_list(rv, input, dataset, stat_pattern, contrast_type)
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
    }, dataset=dataset, contrast_type=contrast_type, fold_split=fold_split)
}

upset_get_plot_list <- function(names_list, comparisons, split_on_fold) {
    plot_list <- names_list
    names(plot_list) <- comparisons %>% gsub("\\.$", "", .)
    if (split_on_fold) {
        plot_list <- lapply(rapply(plot_list, enquote, how="unlist"), eval)
    }
    plot_list
}

upset_get_name_order <- function(plot_list, split_on_fold) {
    if (!split_on_fold) {
        names(plot_list)
    }
    else {
        names(plot_list)[
            c( seq(1,length(names(plot_list)),2),
               seq(2,length(names(plot_list)),2) ) ]
    }
}

upset_get_metadata <- function(plot_list, split_on_fold) {
    if (split_on_fold) {
        list(
            data = data.frame(
                comparison = c(names(plot_list)),
                fold_dir = rep(c("up", "down"), length(plot_list)/2)
            ),
            plots = list(list(
                type = "matrix_rows",
                column = "fold_dir",
                colors = c(up="navy", down="red"),
                alpha=0.2
            ))
        )
    }
    else {
        list(
            data = data.frame(
                comparison = c(names(plot_list)),
                dataset = "d1"
            )
        )
    }
}