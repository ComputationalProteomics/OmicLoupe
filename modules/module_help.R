setup_help_ui <- function(id) {
    ns <- NS(id)
    tabPanel(
        id,
        fluidPage(
            fluidRow(
                column(8,
                       h4("Help: Setup"),
                       plotOutput(ns("setup_image"), height = 1000),
                       p("Text for setup image"),
                       plotOutput(ns("plotly1_image"), height = 1000),
                       p("Text for plotly1 image"),
                       plotOutput(ns("plotly2_image"), height = 1000),
                       p("Text for plotly2 image"),
                       plotOutput(ns("pca_image"), height=1000),
                       p("Text for PCA image"),
                       p("[INSERT] Figure from Setup tab illustrating different aspects"),
                       p("[INSERT] Figure showing how design- and data- matrices belong together"),
                       p("
                         There are three distinct panels: (1) Dataset selection panel (left) 
                         (2) Column selection panel (right)
                         (3) Action panel (middle)."),
                       p("The data selection panel takes a data file and an optional design file.
                         The design file is required to visualize the data as PCA and for doing PCA-based
                         coloring of the statistical plots."),
                       p("The data matrix needs to contain contrast columns containing information on
                         P-values, adjusted p-values, fold change and average expression for features. This
                         should follow the pattern: contrast1.P.Value, contrast1.adj.P.Val, contrast1.logFC,
                         contrast1.AveExpr. Sample columns are required to do PCA plotting"),
                       p("Finally, the action buttons allow moving selecting columns as sample- or stat- columns.
                         This can be performed automatically using the Autodetect button. Then it will look
                         for samples using the 'Select sample column' from the design matrix and look for
                         groups of statistical columns fulfilling the pattern specified above."),
                       p("It is possible to study either one or several datasets. If using multiple a feature
                         column matching between them needs to be specified. The information notice in the middle
                         shows how many entries were present in both cases."),
                       p("CURRENTLY IDS ARE EXPECTED TO BE UNIQUE. THIS WILL BE HANDLED MORE CLEVERLY IN THE FUTURE."),
                       
                       h4("Help: Plotly"),
                       p("[INSERT] Illustrative and marked figure"),
                       p("The main view for Plotly shows six figures and a table. The left column illustrates
                         the reference dataset and its selected comparison. The right the compare dataset.
                         When using threshold filtering features passing these thresholds in the two cases
                         will be illustrated, and also how they are distributed across each other. When using the
                         Column coloring you as a user decide a column from the data matrix that is used for
                         coloring. This can be categorical or continuous, but if having to many discrete values
                         it will be colored black (to avoid crashing). Finally, the figures can be colored based
                         on PCA loadings, meaning they indicate how strongly they contribute to different principal components."),
                       
                       h4("Help: PCA"),
                       p("[INSERT] Illustrative and marked figure"),
                       p("To run the PCA requires a data matrix, a design matrix and that you have mapped sample columns
                         corresponding to the given design matrix column entries")
                )
            )
        )
    )
}

module_help_server <- function(input, output, session) {

    output$setup_image <- renderImage({
        filename <- normalizePath(file.path("./doc", "setup_screen.png"))
        list(src = filename)
    }, deleteFile = FALSE)
    
    output$plotly1_image <- renderImage({
        filename <- normalizePath(file.path("./doc", "plotly_screen1.png"))
        list(src = filename)
    }, deleteFile = FALSE)
    
    output$plotly2_image <- renderImage({
        filename <- normalizePath(file.path("./doc", "plotly_screen1.png"))
        list(src = filename)
    }, deleteFile = FALSE)
    
    output$pca_image <- renderImage({
        filename <- normalizePath(file.path("./doc", "PCA_screen.png"))
        list(src = filename)
    }, deleteFile = FALSE)
    
    
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