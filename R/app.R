get_ui <- function() {
  shiny::navbarPage(
    
    theme = shinythemes::shinytheme("flatly"),
    
    title=sprintf("OmicLoupe v%s", packageVersion("OmicLoupe")),
    id="navbar",
    setup_panel_ui("Setup"),
    
    setup_quality_ui("Quality"),
    setup_pca_ui("PCA"),
    
    setup_plotly_ui("StatDist"),
    setup_overlap_ui("Overlap"),
    
    setup_correlation_ui("Correlation"),
    setup_spotcheck_ui("FeatureCheck")
    
    # setup_help_ui("Help")
  )
}

get_server <- function() {
  shinyServer(function(input, output, session) {
    
    reactive_values <- callModule(module_setup_server, id="Setup", module_name="Setup")
    callModule(module_quality_server, id="Quality", rv=reactive_values, module_name="Quality")
    callModule(module_pca_server, id="PCA", rv=reactive_values, module_name="PCA")
    callModule(module_spotcheck_server, id="FeatureCheck", rv=reactive_values, module_name="FeatureCheck")
    
    callModule(module_statdist_server, id="StatDist", rv=reactive_values, module_name="StatDist", parent_session=session)
    callModule(module_overlap_server, id="Overlap", rv=reactive_values, module_name="Overlap", parent_session=session)
    
    callModule(module_correlation_server, id="Correlation", rv=reactive_values, module_name="Correlation")
    
    # callModule(module_help_server, id="Help", module_name="Help")
    # callModule(module_ideas_server, id="Ideas", module_name="Ideas")
  })
}
