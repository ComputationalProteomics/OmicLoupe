adjust_boxplot <- function(plt, do_violin, rotate_x_labels, order_on_condition, ddf, ddf_sample_col, ddf_cond_col, text_size) {
    
    if (!do_violin) {
        target_geom <- geom_boxplot
    }
    else {
        target_geom <- geom_violin
    }
    
    if (rotate_x_labels) {
        plt <- plt + theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
    }
    
    if (order_on_condition) {
        plt <- plt + scale_x_discrete(
            limits=ddf %>% 
                arrange(UQ(as.name(ddf_cond_col))) %>% 
                dplyr::select(ddf_sample_col) %>% 
                unlist()
        )
    }
    
    plt <- plt + target_geom(na.rm=TRUE) + theme(text=element_text(size=text_size))
    
    plt + xlab("Sample") + ylab("Abundance")
}

make_density_plot <- function(sdf, color, curr_dataset=NULL, title=NULL, text_size=10) {
    plt <- ggplot(sdf, aes_string(x="value", group="name", color=color)) + 
        geom_density(na.rm=TRUE)
    
    if (!is.null(title) && title != "") {
        plt <- plt + ggtitle(title)
    }
    else if (!is.null(curr_dataset) && !is.null(color)) {
        plt <- plt + ggtitle(sprintf("Dataset: %s Color: %s total", curr_dataset, color))
    }
    
    plt <- plt + theme(text = element_text(size=text_size))
    
    plt %>% 
        plotly::ggplotly() %>%
        plotly::layout(xaxis=list(title="Abundance"), yaxis=list(title="Density"))
}

do_dendrogram = function(raw_data_m, raw_color_levels, labels=NULL, pick_top_variance=NULL, title="Dendrogram", omit_samples=NULL, legend_title=NULL, text_size=3, vjust=0.5) {
    
    if (!is.null(omit_samples)) {
        data_m <- raw_data_m[, (!colnames(raw_data_m) %in% omit_samples)]
        color_levels <- raw_color_levels[colnames(raw_data_m) %in% colnames(data_m)]
    }
    else {
        data_m <- raw_data_m
        color_levels <- raw_color_levels
    }
    
    samples <- colnames(data_m)
    
    if (is.null(labels)) {
        labels <- samples
    }
    
    # Setup data
    expr_m_nona <- data_m[complete.cases(data_m),]
    
    # Calculate tree
    scaledTransposedMatrix <- scale(t(expr_m_nona), center=TRUE, scale=TRUE)
    hc <- stats::hclust(stats::dist(scaledTransposedMatrix), "ave")
    dhc <- stats::as.dendrogram(hc)

    # plot_dendro(dhc)
    
    
    # Note - Label order is shuffled within this object! Be careful with coloring.
    ddata <- ggdendro::dendro_data(dhc, type="rectangle")


    # Prepare for plotting
    cluster_label_order <- match(ddata$labels$label, samples)
    ddata$labels$color <- color_levels[cluster_label_order]
    ddata$labels$label <- labels[cluster_label_order]
    
    
    # Visualize
    plt <- ggplot(segment(ddata)) +
        geom_segment(aes(x=.data$x, y=.data$y, xend=.data$xend, yend=.data$yend)) +
        theme_dendro() +
        geom_text(data=label(ddata),
                  aes(x=.data$x, y=.data$y, label=.data$label, color=.data$color),
                  vjust=vjust, hjust=0, size=text_size) +
        coord_flip() +
        scale_y_reverse(expand=c(0.2, 0)) +
        scale_x_continuous(expand=c(0,1)) +
        ggtitle(title)

    if (!is.null(legend_title)) {
        plt <- plt + labs(color = legend_title)
    }

    plt
}

make_barplot <- function(long_sdf, value_col, ddf, sample_col, dataset, color, show_missing=FALSE, rotate_labels=FALSE, text_size=10, title="") {
    
    join_by_ref <- c("name"=sample_col)
    
    summed_data <- long_sdf %>%
        filter(!is.infinite(long_sdf$value)) %>% 
        mutate(is_na=is.na(value)) %>%
        group_by(name) %>%
        summarize_at(c("value", "is_na"), sum, na.rm=TRUE) %>%
        inner_join(ddf, by=join_by_ref)
    
    if (!show_missing) {
        bar_type <- "Summed abundance"
        target_col <- "value"
    }
    else {
        bar_type <- "Number missing"
        target_col <- "is_na"
    }
    
    if (title == "") {
        title <- sprintf("%s Dataset: %s Color: %s", bar_type, dataset, color)
    }
    else {
        title <- title
    }
    
    plt <- ggplot(summed_data, aes_string(x="name", y=target_col, fill=color)) + 
        geom_col() + 
        ggtitle(title) + 
        theme(text=element_text(size=text_size))
    
    if (rotate_labels) {
        plt <- plt + theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
    }
    
    if (!show_missing) {
        plt <- plt + ylab(bar_type)
    }
    else {
        plt <- plt + ylab(bar_type)
    }
    plt + xlab("Sample")
}

