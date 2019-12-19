Venn <- R6Class(
    
    public = list(
        
        simple_venn = function(col1, col2, dereplicate=TRUE, title="Venn", colors=NULL, verbose=FALSE, labels=c("left", "right"), legend.position="right") {
            
            if (dereplicate) {
                col1 <- unique(col1)
                col2 <- unique(col2)
            }
            
            df.venn <- data.frame(
                x = c(-0.5, 0.5), y = c(0, 0),
                labels = labels
            )
            
            joint_count <- length(intersect(col1, col2))
            left_count <- length(col1) - joint_count
            right_count <- length(col2) - joint_count
            # ses$prot_des_prot %>% get_uniques_from_rdf("Protein")
            if (verbose) {
                message("Overlapping features: ", intersect(col1, col2))
                message("Left only features: ", setdiff(col1, col2))
                message("Right only features: ", setdiff(col2, col1))
            }
            
            df.vdc <- data.frame(
                x=c(-1.5, 1.5, 0),
                y=c(0, 0, 0),
                label=c(left_count, right_count, joint_count)
            )
            
            self$do_ggvenn(df.venn, df.vdc, legend.position=legend.position, title=title)
        },
        
        count_venn = function(left_count, right_count, joint_count, title="Venn", colors=NULL, verbose=FALSE, labels=c("left", "right"), legend.position="right") {
            
            df.venn <- data.frame(
                x = c(-0.5, 0.5), y = c(0, 0),
                labels = labels
            )
            
            df.vdc <- data.frame(
                x=c(-1.5, 1.5, 0),
                y=c(0, 0, 0),
                label=c(left_count-joint_count, right_count-joint_count, joint_count)
            )
            
            self$do_ggvenn(df.venn, df.vdc, legend.position=legend.position, title=title)
        },
        
        do_ggvenn = function(df.venn, df.vdc, legend.position='none', colors=NULL, title="No title") {
            
            
            if (is.null(colors)) {
                colors <- c("#A0D398", "#FDF9BD")
            }
            
            plt <- ggplot2::ggplot(data=df.venn) +
                ggforce::geom_circle(
                    ggplot2::aes_string(x0 = "x", y0 = "y", r = 1.5, fill = "labels"),
                    alpha = 0.3,
                    size = 0.5,
                    colour = 'darkgray'
                ) +
                ggplot2::coord_fixed() +
                ggplot2::theme_void() +
                ggplot2::scale_fill_manual(values = colors) +
                ggplot2::scale_colour_manual(values = colors, guide = FALSE) +
                ggplot2::labs(fill = NULL) +
                ggplot2::annotate("text", x = df.vdc$x, y = df.vdc$y, label = df.vdc$label, size = 5) +
                ggplot2::ggtitle(title) + 
                theme(plot.title = element_text(hjust = 0.5))
            
            if (!is.null(labels)) {
                plt <- plt + ggplot2::theme(legend.position = legend.position, legend.direction='vertical')
            }
            
            plt
        },
        
        do_paired_expression_venn = function(col1_w_fold, col2_w_fold, title="", colors=c("#AAAAAA", "#0C81A2")) {

            both_sig_names <- intersect(col1_w_fold %>% names(), col2_w_fold %>% names())
            
            joint_count <- length(both_sig_names)
            joint_contra_count <- length(which(sign(unlist(col1_w_fold[both_sig_names])) != sign(unlist(col2_w_fold[both_sig_names]))))
            joint_same_count <- joint_count - joint_contra_count
            left_count <- length(col1_w_fold) - joint_count
            right_count <- length(col2_w_fold) - joint_count
            
            labels <- c(
                as.character(left_count),
                as.character(right_count),
                sprintf("%s same", joint_same_count),
                sprintf("%s contra", joint_contra_count)
            )
            
            df.vdc <- data.frame(
                x=c(-1.5, 1.5, 0, 0),
                y=c(0, 0, 0, -0.3),
                label=labels
            )
            
            df.venn <- data.frame(x = c(-0.5, 0.5),
                                  y = c(0, 0),
                                  labels = c("Contrast 1", "Contrast 2"), 
                                  stringsAsFactors = FALSE)
            
            plt <- ggplot2::ggplot(data=df.venn) +
                ggforce::geom_circle(
                    ggplot2::aes_string(x0 = "x", y0 = "y", r = 1.5, fill = "labels"), 
                    alpha = 0.3, 
                    size = 0.5, 
                    colour = 'darkgray'
                ) +
                ggplot2::coord_fixed() +
                ggplot2::theme_void() +
                ggplot2::theme(legend.position = 'bottom', legend.direction='vertical') +
                ggplot2::scale_fill_manual(values = colors) +
                ggplot2::scale_colour_manual(values = colors, guide = FALSE) +
                ggplot2::labs(fill = NULL) +
                ggplot2::annotate("text", x = df.vdc$x, y = df.vdc$y, label = df.vdc$label, size = 5) +
                ggplot2::ggtitle(title)
            
            plt
        }
    )
)

venn <- Venn$new()
