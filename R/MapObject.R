MapObject <- R6Class("MapObject", list(
    
    dual_datasets = FALSE,
    dataset1 = NULL,
    target_col1 = NULL,
    
    dataset2 = NULL,
    target_col2 = NULL,
    joint_indices1 = NULL,
    joint_indices2 = NULL,
    has_missing1 = NULL,
    has_missing2 = NULL,
    
    samples1 = NULL,
    samples2 = NULL,

    initialize = function(dataset1, target_col1, dataset2=NULL, target_col2=NULL, samples1=NULL, samples2=NULL) {
        
        # browser()
        self$dataset1 <- dataset1 %>% arrange(UQ(as.name(target_col1)))
        self$target_col1 <- target_col1
        
        if (!is.null(dataset2) && !is.null(target_col2)) {
            self$dual_datasets <- TRUE
            self$dataset2 <- dataset2 %>% arrange(UQ(as.name(target_col2)))
            self$target_col2 <- target_col2
            self$joint_indices1 <- which(self$dataset1[[target_col1]] %in% self$dataset2[[target_col2]])
            self$joint_indices2 <- which(self$dataset2[[target_col2]] %in% self$dataset1[[target_col1]])
        }
        
        if (!is.null(samples1)) {
            if (!all(samples1 %in% colnames(dataset1))) {
                warning("Invalid samples, not all present in colnames")
            }
            self$samples1 <- samples1
        }
        
        if (!is.null(samples2)) {
            if (!all(samples2 %in% colnames(dataset2))) {
                warning("Invalid samples, not all present in colnames")
            }
            self$samples2 <- samples2
        }
    },
    get_full_entries = function(dataset, samples) {
        
        sdf <- dataset[, samples]
        complete.cases(sdf) & !apply(sdf, 1, function(elem) { any(is.infinite(elem)) } )
    },
    get_matching_dataset1 = function() {
        if (!is.null(self$dataset2)) {
            self$dataset1[self$joint_indices1, ]
        }
        else {
            self$dataset1
        }
    },
    get_matching_dataset2 = function() {
        if (!is.null(self$dataset2)) {
            self$dataset2[self$joint_indices2, ]
        }
        else {
            stop("Second dataset not present!")
        }
    },
    has_full_entries = function() {
        # browser()
        if (!is.null(self$dataset1) && !is.null(self$dataset2)) {
            !is.null(self$samples1) && !is.null(self$samples2)
        }
        else if (!is.null(self$dataset1)) {
            !is.null(self$samples1)
        }
        else if (!is.null(self$dataset2)) {
            !is.null(self$samples2)
        } 
        else {
            stop("Unknown situation, for self$dataset1 and self$dataset2: ", self$dataset1, " ", self$dataset2)
        }
    },
    get_combined_dataset = function(full_entries = FALSE) {
        
        if (!is.null(self$dataset1) && !is.null(self$dataset2)) {
            out_df1 <- self$dataset1[self$joint_indices1, ]
            out_df2 <- self$dataset2[self$joint_indices2, ]
            
            if (full_entries) {
                browser()
                out_df1_full_entries <- out_df1 %>% self$get_full_entries(self$samples1)
                out_df2_full_entries <- out_df2 %>% self$get_full_entries(self$samples2)
                all_full_entries <- out_df1_full_entries & out_df2_full_entries
                out_df1 <- out_df1[all_full_entries, ]
                out_df2 <- out_df2[all_full_entries, ]
            }
            
            colnames(out_df1) <- paste0("d1.", colnames(out_df1))
            colnames(out_df2) <- paste0("d2.", colnames(out_df2))
            out_df <- cbind(out_df1, out_df2)
        }
        else if (!is.null(self$dataset1)) {
            out_df <- self$dataset1
            if (full_entries) {
                out_df_full_entries <- out_df %>% self$get_full_entries(self$samples1)
                out_df <- out_df[out_df_full_entries, ]
            }
            colnames(out_df) <- paste0("d1.", colnames(out_df))
            out_df
        }
        else if (!is.null(self$dataset2)) {
            out_df <- self$dataset2
            if (full_entries) {
                out_df_full_entries <- out_df %>% self$get_full_entries(self$samples2)
                out_df <- out_df[out_df_full_entries, ]
            }
            colnames(out_df) <- paste0("d2.", colnames(out_df))
            out_df
        }
        else {
            stop("Unknown situation, for self$dataset1 and self$dataset2: ", self$dataset1, " ", self$dataset2)
        }
        
        cbind(
            comb_id = paste0("C", seq_len(nrow(out_df))),
            out_df
        )
    }
))