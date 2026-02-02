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
  )
}

get_server <- function() {
  shinyServer(function(input, output, session) {
    
    reactive_values <- module_setup_server(id="Setup", module_name="Setup")
    module_quality_server(id="Quality", rv=reactive_values, module_name="Quality")
    module_pca_server(id="PCA", rv=reactive_values, module_name="PCA")
    module_spotcheck_server(id="FeatureCheck", rv=reactive_values, module_name="FeatureCheck")
    
    module_statdist_server(id="StatDist", rv=reactive_values, module_name="StatDist", parent_session=session)
    module_overlap_server(id="Overlap", rv=reactive_values, module_name="Overlap", parent_session=session)

    module_correlation_server(id="Correlation", rv=reactive_values, module_name="Correlation")
  })
}
