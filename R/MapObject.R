library(R6)

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
    
    correlations = NULL,

    initialize = function(dataset1, target_col1, dataset2=NULL, target_col2=NULL, samples1=NULL, samples2=NULL, matched=FALSE) {
        
        self$dataset1 <- dataset1 %>% arrange(UQ(as.name(target_col1)))
        self$target_col1 <- target_col1
        
        if (!is.null(dataset2) && !is.null(target_col2)) {
            self$dual_datasets <- TRUE
            self$dataset2 <- dataset2 %>% arrange(UQ(as.name(target_col2)))
            self$target_col2 <- target_col2
            self$joint_indices1 <- which(self$dataset1[[target_col1]] %in% self$dataset2[[target_col2]])
            self$joint_indices2 <- which(self$dataset2[[target_col2]] %in% self$dataset1[[target_col1]])
        }
        
        if (!is.null(samples1) && length(samples1) > 0) {
            if (!all(samples1 %in% colnames(dataset1))) {
                warning("Invalid samples, not all present in colnames")
            }
            self$samples1 <- samples1
        }
        
        if (!is.null(samples2) && length(samples2) > 0) {
            if (!all(samples2 %in% colnames(dataset2))) {
                warning("Invalid samples, not all present in colnames")
            }
            self$samples2 <- samples2
        }
        
        if (matched) {
            
            if (length(self$samples1) != length(self$samples2)) {
                warning(sprintf("Number of samples (%s and %s) does not match, not calculating correlations", length(self$samples1), length(self$samples2)))
            }
            else {            
                message("Calculating correlations")
                ref_rdf <- self$dataset1
                comp_rdf <- self$dataset2
                
                ref_id_col <- ref_rdf %>% dplyr::select(self$target_col1) %>% unlist() %>% unname()
                comp_id_col <- comp_rdf %>% dplyr::select(self$target_col2) %>% unlist() %>% unname()
                
                joint_ids <- ref_id_col[ref_id_col %in% comp_id_col]
                
                ref_rdf_joint <- ref_rdf %>% 
                    filter(UQ(as.name(self$target_col1)) %in% joint_ids) %>% 
                    arrange(UQ(as.name(self$target_col1)))
                comp_rdf_joint <- comp_rdf %>% 
                    filter(UQ(as.name(self$target_col2)) %in% joint_ids) %>% 
                    arrange(UQ(as.name(self$target_col2)))
                
                ref_sdf_joint <- ref_rdf_joint %>% dplyr::select(self$samples1)
                comp_sdf_joint <- comp_rdf_joint %>% dplyr::select(self$samples2)
                
                corr_types <- c("pearson", "spearman", "kendall")
                corrs <- lapply(
                    corr_types,
                    function(corr_type) {
                        pearson_corr <- mapply(
                            cor,
                            ref_sdf_joint %>% t() %>% data.frame(),
                            comp_sdf_joint %>% t() %>% data.frame(),
                            MoreArgs = list("use" = "pairwise.complete.obs", "method" = corr_type)
                        )
                    }
                )
                names(corrs) <- corr_types
                self$correlations <- corrs
                
            }
            

        }
    },
    has_correlations = function() {
        !is.null(self$correlations)
    },
    get_full_entries = function(dataset, samples) {
        
        sdf <- dataset[, samples]
        complete.cases(sdf) & !apply(sdf, 1, function(elem) { any(is.infinite(elem)) } )
    },
    get_dataset1_nrow = function() {
        self$dataset1 %>% nrow()
    },
    get_dataset2_nrow = function() {
        self$dataset2 %>% nrow()
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
    has_combined = function() {
        length(self$joint_indices1) > 0 && length(self$joint_indices1) == length(self$joint_indices2)
    },
    get_combined_dataset = function(full_entries = FALSE) {
    
        if (!is.null(self$dataset1) && !is.null(self$dataset2)) {
            
            if (!self$has_combined()) {
                return(NULL)
            }
                
            out_df1 <- self$dataset1[self$joint_indices1, ]
            out_df2 <- self$dataset2[self$joint_indices2, ]

            if (!is.null(self$correlations)) {
                out_df2 <- cbind(out_df2, do.call("cbind", self$correlations))
            }
                        
            if (full_entries) {
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
        
        if (nrow(out_df) > 0) {
            cbind(
                comb_id = paste0("C", seq_len(nrow(out_df))),
                out_df
            )
        } 
        else {
            NULL
        }
    }
))