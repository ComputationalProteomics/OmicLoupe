library(ggplot2)
library(PCAtools)

setup_pca_ui <- function(id) {
    ns <- NS(id)
    tabPanel(
        id,
        fluidPage(
            fluidRow(
                column(4,
                       # data_display_ui_panel(ns)
                       numericInput(ns("pc_comp_1_data1"), "PC1", min=1, value=1, step=1),
                       numericInput(ns("pc_comp_2_data1"), "PC2", min=1, value=2, step=1),
                       fluidRow(
                           column(8, selectInput(ns("color_data1"), "Color", choices=c(""))),
                           column(4, checkboxInput(ns("use_color_data1"), "Use", value=FALSE))
                       ),
                       fluidRow(
                           column(8, selectInput(ns("shape_data1"), "Shape", choices=c(""))),
                           column(4, checkboxInput(ns("use_shape_data1"), "Use", value=FALSE))
                       ),
                       numericInput(ns("dot_size"), "Dot size", min=1, value=3, step=1),
                       checkboxInput(ns("scale_pca_data1"), "Scale", value = TRUE),
                       checkboxInput(ns("center_pca_data1"), "Center", value = TRUE),
                       checkboxInput(ns("show_labels_data1"), "Show labels", value = FALSE),
                       checkboxInput(ns("show_loadings_data1"), "Show loadings", value = FALSE),
                       numericInput(ns("variance_filter_data1"), "Variance filter", min=0, max=1, step=0.01, value = 0.1)
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
    
    observeEvent(reactive_vals$design_1(), {
        print("Observed design change")
        updateSelectInput(session, "color_data1", choices = colnames(reactive_vals$design_1()))
        updateSelectInput(session, "shape_data1", choices = colnames(reactive_vals$design_1()))
    })
    
    output$pca_plot <- renderPlot({
        
        rdf <- reactive_vals$filedata_1()
        ddf <- reactive_vals$design_1()
        
        # browser()
        
        warning("Update sample selection here with reference dataset (if applied)")
        
        # 1 -> reference dataset
        samples <- reactive_vals$selected_cols_obj()[[1]]$samples
        sdf <- rdf[, samples]
        sdf_complete <- sdf[complete.cases(sdf), ] %>% t()
        pca_obj <- prcomp(sdf_complete, scale. = input$scale_pca_data1, center=input$center_pca_data1)
        pc1 <- sprintf("PC%s", input$pc_comp_1_data1)
        pc2 <- sprintf("PC%s", input$pc_comp_2_data1)
        
        if (input$use_color_data1) color <- input$color_data1
        else color <- NULL

        if (input$use_shape_data1) shape <- input$shape_data1
        else shape <- NULL
        
        plt_df <- cbind(pca_obj$x, ddf)
        
        plt <- ggplot(plt_df, aes_string(x=pc1, y=pc2, color=color, shape=shape)) + 
            geom_point(size=input$dot_size)
        
        # pca_obj$rotation - loadings
        # pca_obj$x - scores
        
        # Histogram of loadings
        # plt <- ggplot(as.data.frame(pca_obj$rotation), aes(x=PC4)) + geom_histogram(bins=30)
        # ggsave(plt, filename = "~/Desktop/test.png")
        
        # The rotation values could be extracted, and linked to different comparisons
        
        plt
    })
    
}








