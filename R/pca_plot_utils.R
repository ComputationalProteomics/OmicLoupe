make_pca_plt <- function(ddf, pca_obj, pc1, pc2, color, shape, sample, label_col, title_label="No title set", dot_size=3, show_labels=FALSE, color_as_fact=FALSE, text_size=10) {
    
    if (!is.numeric(pc1) || length(pc1) != 1 || is.na(pc1) || pc1 < 1) {
        stop("pc1 must be a single positive number.")
    }
    if (!is.numeric(pc2) || length(pc2) != 1 || is.na(pc2) || pc2 < 1) {
        stop("pc2 must be a single positive number.")
    }

    max_pcs <- ncol(pca_obj$x)
    if (pc1 > max_pcs || pc2 > max_pcs) {
        stop(sprintf(
            "Requested PC%s/PC%s but PCA only has %s components. Choose PCs between 1 and %s.",
            pc1, pc2, max_pcs, max_pcs
        ))
    }
    if (nrow(pca_obj$x) != nrow(ddf)) {
        stop(sprintf(
            "PCA scores have %s samples but design data has %s rows. Ensure the design matrix matches the selected samples.",
            nrow(pca_obj$x), nrow(ddf)
        ))
    }

    required_cols <- c(sample, label_col, color, shape)
    required_cols <- required_cols[!is.null(required_cols)]
    required_cols <- required_cols[required_cols != ""]
    missing_cols <- setdiff(required_cols, names(ddf))
    if (length(missing_cols) > 0) {
        stop(sprintf(
            "Missing required column(s) in design data: %s",
            paste(missing_cols, collapse = ", ")
        ))
    }

    pc1_lab <- sprintf("PC%s", pc1)
    pc2_lab <- sprintf("PC%s", pc2)
    
    pc1_var <- pca_obj$sdev[pc1] ** 2 / sum(pca_obj$sdev ** 2)
    pc2_var <- pca_obj$sdev[pc2] ** 2 / sum(pca_obj$sdev ** 2)
    
    plt_df <- cbind(pca_obj$x, ddf)
    if (!is.null(shape)) {
        plt_df[[shape]] <- as.factor(plt_df[[shape]])
    }
    if (color_as_fact && !is.null(color)) {
        plt_df[[color]] <- as.factor(plt_df[[color]])
    }
    
    plt_base <- ggplot(
        plt_df,
        aes(
            x = .data[[pc1_lab]],
            y = .data[[pc2_lab]],
            text = .data[[sample]],
            label = .data[[label_col]]
        )
    )
    if (!is.null(color)) {
        plt_base <- plt_base + aes(colour = .data[[color]])
    }
    if (!is.null(shape)) {
        plt_base <- plt_base + aes(shape = .data[[shape]])
    }
    if (!show_labels) {
        plt_base <- plt_base + geom_point(size=dot_size)
    }
    else {
        plt_base <- plt_base + geom_text(size=dot_size)
    }
    
    plt_base + 
        ggtitle(sprintf("Dataset: %s (dim: %s)", title_label, paste(dim(pca_obj$rotation), collapse=", "))) +
        xlab(sprintf("PC%s (%s %s)", pc1, round(pc1_var * 100, 2), "%")) +
        ylab(sprintf("PC%s (%s %s)", pc2, round(pc2_var * 100, 2), "%")) +
        theme(text=element_text(size=text_size), legend.title = element_blank())
}

make_pair_pca_plot <- function(ddf, pca_obj, color, color_as_fact=FALSE, pcs) {
    
    if (!is.numeric(pcs) || length(pcs) != 1 || is.na(pcs) || pcs < 1) {
        stop("pcs must be a single positive number.")
    }

    max_pcs <- ncol(pca_obj$x)
    if (pcs > max_pcs) {
        stop(sprintf(
            "Requested %s PCs but PCA only has %s components. Choose a value between 1 and %s.",
            pcs, max_pcs, max_pcs
        ))
    }
    if (nrow(pca_obj$x) != nrow(ddf)) {
        stop(sprintf(
            "PCA scores have %s samples but design data has %s rows. Ensure the design matrix matches the selected samples.",
            nrow(pca_obj$x), nrow(ddf)
        ))
    }

    plt_df <- cbind(pca_obj$x, ddf)
    pcs <- paste0("PC", 1:pcs)
    if (!is.null(color)) {
        if (color_as_fact) {
            plt_df[[color]] <- as.factor(plt_df[[color]])
        }
        plt_df %>%
            dplyr::select(dplyr::all_of(c(pcs, color))) %>%
            ggpairs(aes(color=.data[[color]], alpha=0.5))
    } else {
        plt_df %>%
            dplyr::select(dplyr::all_of(pcs)) %>%
            ggpairs()
    }
}
