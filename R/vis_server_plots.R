two_level_colors <- c("#CCCCCC", "#CC0000", "#0000CC")
two_level_colors_other <- c("#CCCCCC", "#0000CC")

# Generating correlation based informative text
calculate_correlation_vals_string <- function(target_data, ref_stat_cols, comp_stat_cols, pvalue_cutoff=0.05, fold_cutoff=1, pvalue_adjusted=FALSE) {
    
    plot_df <- get_pass_thres_annot_data(
        target_data,
        ref_stat_cols,
        pvalue_cutoff, 
        fold_cutoff,
        pvalue_adjusted
    ) %>% dplyr::filter(pass_threshold_data)
    
    # Fold changes
    pval_spearman <- cor.test(plot_df[[ref_stat_cols$P.Value]], plot_df[[comp_stat_cols$P.Value]], method="spearman")
    fold_spearman <- cor.test(plot_df[[ref_stat_cols$logFC]], plot_df[[comp_stat_cols$logFC]], method="spearman")
    fold_pearson <- cor.test(plot_df[[ref_stat_cols$logFC]], plot_df[[comp_stat_cols$logFC]], method="pearson")
    
    out_string <- sprintf(
        "P-value ranked correlation: %s (p-val: %s)\nFold-ranked correlation: %s (p-val: %s)\nFold-Spearman correlation: %s (p-val: %s)\nNumber of features considered: %s out of %s",
        round(pval_spearman$estimate, 3), round(pval_spearman$p.value, 3),
        round(fold_spearman$estimate, 3), round(fold_spearman$p.value, 3),
        round(fold_pearson$estimate, 3), round(fold_pearson$p.value, 3),
        nrow(plot_df), nrow(target_data)
    )
    
    out_string
}

# correlation_curve <- function(target_data, stat_cols1, stat_cols2, ref_stat_cols, ) {
#     
# }

minimap_hist <- function(dataset, target_col, pvalue_cutoff, bin_count, label="(No label)") {
    ggplot(dataset, aes_string(x=target_col)) + 
        geom_histogram(bins=bin_count, na.rm = TRUE) +
        geom_vline(xintercept=pvalue_cutoff) + 
        ggtitle(sprintf("%s distribution", label))
}

pvaluehists <- function(plot_df, stat_cols1, stat_cols2, stat_base1, stat_base2, bin_count) {

    plt1 <- ggplot(plot_df, aes_string(x=stat_cols1$P.Value, fill="pass_threshold_data")) + 
        geom_histogram(bins=bin_count, na.rm=TRUE) + 
        scale_fill_manual(values=two_level_colors) + 
        ggtitle(paste(stat_base1, " p-value histogram"))
    
    plt2 <- ggplot(plot_df, aes_string(x=stat_cols2$P.Value, fill="pass_threshold_data")) + 
        geom_histogram(bins=bin_count, na.rm=TRUE) + 
        scale_fill_manual(values=two_level_colors) + 
        ggtitle(paste(stat_base2, " p-value histogram"))
    
    ggarrange(plt1, plt2, nrow=2, ncol=1)
}

scatterplots <- function(plot_df, stat_cols1, stat_cols2, stat_base1, stat_base2, mode="MA") {

    if (mode == "ma") {
        x_expr <- function(stat_cols) stat_cols$AveExpr
        y_expr <- function(stat_cols) stat_cols$logFC
    }
    else if (mode == "volcano") {
        plot_df$stat1_log10pval <- -log10(plot_df[[stat_cols1$P.Value]])
        plot_df$stat2_log10pval <- -log10(plot_df[[stat_cols2$P.Value]])
        stat_cols1$minlog10pval <- "stat1_log10pval"
        stat_cols2$minlog10pval <- "stat2_log10pval"
        x_expr <- function(stat_cols) stat_cols$logFC
        y_expr <- function(stat_cols) stat_cols$minlog10pval
    }
    else {
        stop("Unknown mode: ", mode)
    }

    plt1 <- ggplot(plot_df, aes_string(x=x_expr(stat_cols1), y=y_expr(stat_cols1), color="pass_threshold_data")) + 
        geom_point(na.rm = TRUE) + 
        scale_color_manual(values=two_level_colors) + 
        ggtitle(paste(stat_base1, mode))
    
    plt2 <- ggplot(plot_df, aes_string(x=x_expr(stat_cols2), y=y_expr(stat_cols2), color="pass_threshold_data")) + 
        geom_point(na.rm = TRUE) + 
        scale_color_manual(values=two_level_colors) + 
        ggtitle(paste(stat_base2, mode))
    
    return(list(plt1, plt2))
    
    # ggarrange(plt1, plt2, nrow=2, ncol=1)
    
}

custom_comp_plot <- function(rdf, stat_cols1, stat_cols2, pvalue_type, pvalue_cutoff) {
    rdf$fold_diff <- rdf[[stat_cols1$logFC]] - rdf[[stat_cols2$logFC]]
    rdf$is_contra <- sign(rdf[[stat_cols1$logFC]]) != sign(rdf[[stat_cols2$logFC]])
    
    plt1 <- ggplot(rdf, aes_string(x=stat_cols1$P.Value, y="fold_diff", color="is_contra")) + 
        geom_point(na.rm=TRUE) + 
        ggtitle("Fold diffs, full dataset") + 
        scale_color_manual(values=two_level_colors_other)
    
    plt2 <- ggplot(rdf, aes_string(x=stat_cols1[[pvalue_type]], y="fold_diff", color="is_contra")) + 
        geom_point(na.rm=TRUE) + 
        xlim(0, pvalue_cutoff) + 
        ggtitle("Fold diffs, selected significance range") + 
        scale_color_manual(values=two_level_colors_other)
    
    ggarrange(plt1, plt2, nrow=2, ncol=1)
}

exact_fold_comp_plot <- function(rdf, group1_cols, group2_cols, stat_target, stat_base1, stat_base2, pvalue_type, pvalue_cutoff) {

    long <- rdf %>% 
        dplyr::filter(UQ(as.name(group1_cols[[pvalue_type]])) < pvalue_cutoff) %>%
        mutate(id = paste("ID", row_number(), sep="")) %>% 
        dplyr::select(
            id, 
            fold1=UQ(as.name(group1_cols$logFC)), 
            fold2=UQ(as.name(group2_cols$logFC)), 
            sig_val=UQ(as.name(group1_cols[[pvalue_type]])), 
            expr=UQ(as.name(group1_cols$AveExpr))
        ) %>% 
        gather("fold", "value", -id, -sig_val, -expr)
    
    plt <- ggplot(long, aes(x=sig_val, y=value+expr, color=fold, group=id)) + 
        geom_line(color="gray", na.rm=TRUE) + 
        geom_point(na.rm=TRUE) + 
        scale_color_manual(values=two_level_colors_other) +
        ggtitle("Fold diffs, in absolute values (approximated around average gene level)") +
        xlab(paste0(stat_base1, pvalue_type)) +
        ylab("Expression levels")
    plt

}









