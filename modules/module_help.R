setup_help_ui <- function(id) {
    ns <- NS(id)
    tabPanel(
        id,
        fluidPage(
            fluidRow(
                tabsetPanel(
                    id = ns("help_panels"),
                    type = "tabs",
                    tabPanel(
                        "Input data formats",
                        htmlOutput(ns("input_data_text"))
                    ),
                    tabPanel(
                        "Setup page", 
                        htmlOutput(ns("setup_image_text")),
                        plotOutput(ns("setup_image"), height = 800)
                    ),
                    tabPanel(
                        "Statistics page",
                        htmlOutput(ns("plotly1_image_text")),
                        plotOutput(ns("plotly1_image"), height = 1050)
                    ),
                    tabPanel(
                        "Statistics page 2",
                        htmlOutput(ns("plotly2_image_text")),
                        plotOutput(ns("plotly2_image"), height = 1050)
                    ),
                    tabPanel(
                        "PCA page",
                        htmlOutput(ns("pca_image_text")),
                        plotOutput(ns("pca_image"), height=1000)
                    )
                )
            )
        )
    )
}

module_help_server <- function(input, output, session, module_name) {

    output$setup_image <- renderImage({
        filename <- normalizePath(file.path("./doc", "setup_screen.png"))
        list(src = filename)
    }, deleteFile = FALSE)
    
    output$setup_image_text <- renderUI({
        HTML(parse_vector_to_bullets(c(
            "Select dataset 1 or 2",
            "Upload tab delimited data file containing sample columns and optionally annotation",
            "Column names will automatically appear here on upload",
            "Optional design matrix with annotation for each sample",
            "What column contains sample names - should be exact match to what is found in the data file",
            "Select or deselect samples manually by marking in the left/right panels and moving with arrow buttons",
            "Select or deselect statistics columns manually",
            "Detect sample columns and statistics columns automatically based on patterns matching the pattern 'pattern.P.Value$'. Sample columns are retrieved from the design matrix.",
            "Information about mapping between datasets",
            "Select sample columns",
            "Selected statistics columns",
            "The statistic columns are expected to follow the pattern 'base.P.Value', 'base.adj.P.Val', 'base.logFC' and 'base.AvgExpr'. This displays all patterns matching this.",
            "Column among data columns containing unique identifier which can be used to map across datasets"
        )))
    })
    
    output$plotly1_image <- renderImage({
        filename <- normalizePath(file.path("./doc", "plotly_screen1.png"))
        list(src = filename)
    }, deleteFile = FALSE)
    
    output$plotly1_image_text <- renderUI({
        HTML(parse_vector_to_bullets(c(
            "First",
            "Second"
        )))
    })
    
    output$plotly2_image <- renderImage({
        filename <- normalizePath(file.path("./doc", "plotly_screen2.png"))
        list(src = filename)
    }, deleteFile = FALSE)
    
    output$plotly2_image_text <- renderUI({
        HTML(parse_vector_to_bullets(c(
            "First",
            "Second"
        )))
    })
    
    output$pca_image <- renderImage({
        filename <- normalizePath(file.path("./doc", "PCA_screen.png"))
        list(src = filename)
    }, deleteFile = FALSE)
    
    output$pca_image_text <- renderUI({
        HTML(parse_vector_to_bullets(c(
            "First",
            "Second"
        )))
    })
    
    parse_vector_to_bullets <- function(vect, number=TRUE) {
        html_string <- paste0(
            "<li>",
            paste(vect, collapse="</li><li>"),
            "</li>"
        )
        
        if (!number) {
            list_style <- "ul"
        }
        else {
            list_style <- "ol"
        }
        
        sprintf("<%s>%s</%s>", list_style, html_string, list_style)
    }
    
    
    # output$help_setup <- renderText({
    #     "There are three separate panels."
    # })
    # 
    # output$help_plotly <- renderText({
    #     "Plotly text"
    # })
    # 
    # output$help_pca <- renderText({
    #     "PCA text"
    # })

}