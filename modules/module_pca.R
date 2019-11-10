library(ggplot2)
library(PCAtools)

setup_pca_ui <- function(id) {
    ns <- NS(id)
    tabPanel(
        id,
        fluidPage(
            fluidRow(
                column(4,
                       data_display_ui_panel(ns)
                ),
                column(8,
                       p("test"),
                       textOutput(ns("text_out")),
                       plotOutput(ns("pca_plot"))
                )
            )
        )
    )
}

module_pca_server <- function(input, output, session, reactive_vals) {
    
    output$text_out <- renderText({
        "out text"
    })
    
    output$pca_plot <- renderPlot({
        
        rdf <- reactive_vals$filedata_1()
        warning("Update sample selection here with reference dataset (if applied)")
        samples <- reactive_vals$selected_cols_obj()[[1]]$samples
        sdf <- rdf[, samples]
        sdf_complete <- sdf[complete.cases(sdf), ] %>% t()
        pca_obj <- prcomp(sdf_complete, scale. = TRUE, center=TRUE)
        # pca_obj$rotation - loadings
        # pca_obj$x - scores
        plt <- ggplot(as.data.frame(pca_obj$x), aes(x=PC1, y=PC2)) + geom_point()
        
        # Histogram of loadings
        # plt <- ggplot(as.data.frame(pca_obj$rotation), aes(x=PC4)) + geom_histogram(bins=30)
        # ggsave(plt, filename = "~/Desktop/test.png")
        
        # The rotation values could be extracted, and linked to different comparisons
        
        plt
    })
    
}








