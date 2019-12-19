setup_overlap_ui <- function(id) {
    ns <- NS(id)
    tabPanel(
        id,
        fluidPage(
            fluidRow(
                column(8,
                       h4("Some ideas for development"),
                       htmlOutput(ns("html"))
                )
            ),
            p("Left and right: Venns or upsets"),
            fluidRow(
                column(
                    12,
                    wellPanel(
                        fluidRow(
                            column(4,
                                   selectInput(ns("dataset1"), "Reference dataset", choices = c("Dev"), selected = "Dev"),
                                   selectInput(ns("dataset2"), "Comp. dataset", choices = c("Dev"), selected = "Dev")
                            ),
                            column(4,
                                   selectInput(ns("ref_contrast"), "Ref. contr.", choices = c("Dev"), selected = "Dev"),
                                   selectInput(ns("comp_contrast"), "Comp. contr.", choices = c("Dev"), selected = "Dev")
                            ),
                            column(4,
                                   selectInput(ns("ref_contr_type"), "Ref. contr. type", choices = c("Dev"), selected = "Dev"),
                                   selectInput(ns("comp_contr_type"), "Comp. contr. type", choices = c("Dev"), selected = "Dev")
                            )
                        )
                    ),
                    htmlOutput(ns("warnings")),
                    tabsetPanel(
                        id = ns("plot_tabs"),
                        type = "tabs",
                        tabPanel("Venn",
                                 plotOutput(ns("venn_ref")),
                                 plotOutput(ns("venn_comp"))
                        ),
                        tabPanel("Upset",
                                plotOutput(ns("upset_ref")),
                                plotOutput(ns("upset_comp"))
                        )
                    )
                )
            )
        )
    )
}

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

module_overlap_server <- function(input, output, session, rv) {
    
    observeEvent(rv$filedata_1(), {
        choices <- get_dataset_choices(rv)
        updateSelectInput(session, "dataset1", choices=choices, selected=choices[1])
        updateSelectInput(session, "dataset2", choices=choices, selected=choices[1])
    })
    
    observeEvent(rv$filedata_2(), {
        choices <- get_dataset_choices(rv)
        updateSelectInput(session, "dataset1", choices=choices, selected=choices[1])
        updateSelectInput(session, "dataset2", choices=choices, selected=choices[1])
    })
    
    sync_param_choices <- function() {
        ref_choices <- c("None", rv$ddf_cols_ref(rv, input$dataset1))
        comp_choices <- c("None", rv$ddf_cols_comp(rv, input$dataset2))
        # updateSelectInput(session, "color_data_ref", choices = ref_choices, selected=ref_choices[1])
        # updateSelectInput(session, "sample_data1", choices = ref_choices, selected=ref_choices[1])
        # updateSelectInput(session, "color_data_comp", choices = comp_choices, selected=comp_choices[1])
        # updateSelectInput(session, "sample_data2", choices = comp_choices, selected=comp_choices[1])

        ref_data_choices <- c("None", rv$rdf_cols_ref(rv, input$dataset1))
        comp_data_choices <- c("None", rv$rdf_cols_comp(rv, input$dataset2))
        updateSelectInput(session, "data_num_col_ref", choices = ref_data_choices, selected=ref_data_choices[1])
        updateSelectInput(session, "data_cat_col_ref", choices = ref_data_choices, selected=ref_data_choices[1])
        updateSelectInput(session, "data_num_col_comp", choices = comp_data_choices, selected=comp_data_choices[1])
        updateSelectInput(session, "data_cat_col_comp", choices = comp_data_choices, selected=comp_data_choices[1])
    }

    observeEvent(rv$ddf_ref(rv, input$dataset1), {
        sync_param_choices()
    })

    observeEvent(rv$ddf_comp(rv, input$dataset2), {
        sync_param_choices()
    })
    
    output$warnings <- renderUI({
        
        error_vect <- c()
        if (is.null(rv$filename_1())) {
            error_vect <- c(error_vect, "No filename_1 found, upload dataset at Setup page")
        }
        
        if (is.null(rv$design_1())) {
            error_vect <- c(error_vect, "No design_1 found, upload dataset at Setup page")
        }
        
        total_text <- paste(error_vect, collapse="<br>")
        HTML(sprintf("<b><font size='5' color='red'>%s</font></b>", total_text))
    })
    
    output$html <- renderUI({
        
        HTML(parse_vector_to_bullets(c(
            "Illustration of presence of certain features across the two datasets",
            "Illustration of presence of certain features in certain statistical comparisons",
            "Illustration of overlap across features passing certain statistical thresholds",
            "Venn diagrams and upsets are alternatives"
        )))
    })
}

