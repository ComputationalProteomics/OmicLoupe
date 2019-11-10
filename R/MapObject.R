MapObject <- R6Class("MapObject", list(
    
    dual_datasets = FALSE,
    dataset1 = NULL,
    target_col1 = NULL,
    
    dataset2 = NULL,
    target_col2 = NULL,
    joint_indices1 = NULL,
    joint_indices2 = NULL,

    initialize = function(dataset1, target_col1, dataset2=NULL, target_col2=NULL) {
        
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
    get_combined_dataset = function() {
        if (!is.null(self$dataset1) && !is.null(self$dataset2)) {
            # browser()
            # warning("Not sure whether indices are in proper order yet, needs to be fixed!")
            out_df1 <- self$dataset1[self$joint_indices1, ]
            out_df2 <- self$dataset2[self$joint_indices2, ]
            colnames(out_df1) <- paste0("d1.", colnames(out_df1))
            colnames(out_df2) <- paste0("d2.", colnames(out_df2))
            cbind(out_df1, out_df2)
        }
        else if (!is.null(self$dataset1)) {
            out_df1 <- self$dataset1
            colnames(out_df1) <- paste0("d1.", colnames(out_df1))
            out_df1
        }
        else if (!is.null(self$dataset2)) {
            out_df2 <- self$dataset2
            colnames(out_df2) <- paste0("d2.", colnames(out_df2))
            out_df2
        }
        else {
            stop("Unknown situation, for self$dataset1 and self$dataset2: ", self$dataset1, " ", self$dataset2)
        }
    }
))