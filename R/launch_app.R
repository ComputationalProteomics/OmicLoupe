#' launches the shinyAppDemo app
#'
#' @param data Optional data frame for single-dataset mode (alias of \code{data1}).
#' @param design Optional design matrix for single-dataset mode (alias of \code{design1}).
#' @param data1 Optional primary data frame.
#' @param data2 Optional secondary data frame.
#' @param design1 Optional primary design matrix.
#' @param design2 Optional secondary design matrix.
#' @param feature_col Optional feature ID column name for single-dataset mode (alias of \code{feature_col1}).
#' @param feature_col1 Optional feature ID column name for \code{data1}.
#' @param feature_col2 Optional feature ID column name for \code{data2}.
#' @param sample_col Column name in the design matrix identifying samples (used as default for \code{sample_col1} and \code{sample_col2}).
#' @param sample_col1 Optional sample column name for \code{design1}.
#' @param sample_col2 Optional sample column name for \code{design2}.
#' @param matched_samples Logical; whether samples are matched between datasets.
#' @param skip_correlation Logical; whether to skip correlation calculations.
#' @param discard_duplicates Logical; whether to discard duplicate feature IDs when mapping.
#' @param auto_load Logical; whether to auto-load preloaded data on startup.
#'
#' @export runApp
#'
#' @return shiny application object
#'
#' @importFrom ggdendro theme_dendro segment label
#' @import dplyr
#' @importFrom forcats fct_collapse
#' @importFrom ggforce geom_circle
#' @import ggplot2
#' @importFrom ggpubr ggarrange
#' @importFrom jsonlite toJSON
#' @importFrom plotly ggplotly plot_ly renderPlotly toWebGL plotlyOutput config event_data
#' @importFrom purrr walk map discard keep
#' @importFrom R6 R6Class
#' @importFrom readr read_tsv cols write_tsv
#' @importFrom GGally ggpairs
#' @import shiny
#' @importFrom shinyalert shinyalert
#' @importFrom shinycssloaders withSpinner
#' @importFrom stats complete.cases cor.test prcomp quantile reorder setNames var
#' @importFrom stringr str_length str_split str_length str_trunc
#' @importFrom tidyr pivot_longer gather unite
#' @importFrom utils head packageVersion
runApp <- function(
    data = NULL,
    design = NULL,
    data1 = NULL,
    data2 = NULL,
    design1 = NULL,
    design2 = NULL,
    feature_col = NULL,
    feature_col1 = NULL,
    feature_col2 = NULL,
    sample_col = "sample",
    sample_col1 = NULL,
    sample_col2 = NULL,
    matched_samples = FALSE,
    skip_correlation = FALSE,
    discard_duplicates = FALSE,
    auto_load = TRUE
) {

    if (!is.null(data) && !is.null(data1)) {
        stop("Cannot specify both 'data' and 'data1'. Use 'data' for single dataset or 'data1'/'data2' for two datasets.")
    }
    if (!is.null(design) && !is.null(design1)) {
        stop("Cannot specify both 'design' and 'design1'. Use 'design' for single dataset or 'design1'/'design2' for two datasets.")
    }
    if (!is.null(feature_col) && !is.null(feature_col1)) {
        stop("Cannot specify both 'feature_col' and 'feature_col1'. Use 'feature_col' for single dataset.")
    }

    if (!is.null(data)) data1 <- data
    if (!is.null(design)) design1 <- design
    if (!is.null(feature_col)) feature_col1 <- feature_col

    if (is.null(sample_col1)) sample_col1 <- sample_col
    if (is.null(sample_col2)) sample_col2 <- sample_col

    two_datasets <- !is.null(data2)

    if (!is.null(data1)) {
        validate_preloaded_data(
            data1 = data1,
            data2 = data2,
            design1 = design1,
            design2 = design2,
            feature_col1 = feature_col1,
            feature_col2 = feature_col2,
            sample_col1 = sample_col1,
            sample_col2 = sample_col2,
            two_datasets = two_datasets
        )

        if (is.null(feature_col1)) {
            feature_col1 <- detect_feature_column(data1)
            message(sprintf("Auto-detected feature column for data1: '%s'", feature_col1))
        }
        if (two_datasets && is.null(feature_col2)) {
            feature_col2 <- detect_feature_column(data2)
            message(sprintf("Auto-detected feature column for data2: '%s'", feature_col2))
        }

        options(omicloupe.preloaded = list(
            data1 = data1,
            data2 = data2,
            design1 = design1,
            design2 = design2,
            feature_col1 = feature_col1,
            feature_col2 = feature_col2,
            sample_col1 = sample_col1,
            sample_col2 = sample_col2,
            two_datasets = two_datasets,
            matched_samples = matched_samples,
            skip_correlation = skip_correlation,
            discard_duplicates = discard_duplicates,
            auto_load = auto_load
        ))

        message("Launching OmicLoupe with pre-loaded data...")
	    }

	    old_useragg <- shiny::getShinyOption("useragg", default = NULL)
	    if (!requireNamespace("textshaping", quietly = TRUE)) {
	        shiny::shinyOptions(useragg = FALSE)
	    }

	    options(shiny.maxRequestSize=MAX_FILE_SIZE_MB*1024^2)
	    ggplot2::theme_set(ggplot2::theme_classic())

	    message(sprintf("Running OmicLoupe version %s", packageVersion("OmicLoupe")))

	    onStop <- function() {
	        options(omicloupe.preloaded = NULL)
	        if (is.null(old_useragg)) {
	            shiny::shinyOptions(useragg = NULL)
	        } else {
	            shiny::shinyOptions(useragg = old_useragg)
	        }
	    }

    server <- get_server()
    server_with_cleanup <- function(input, output, session) {
        shiny::onStop(onStop, session = session)
        server(input, output, session)
    }

    shinyApp(
        ui = get_ui(),
        server = server_with_cleanup
    )
}


if(getRversion() >= "2.15.1")  utils::globalVariables(c("."))
