make_mock_pca_rv <- function(rdf, ddf) {
    list(
        setup_input = function() list(),
        filedata_1 = function() rdf,
        filedata_2 = function() rdf,
        filename_1 = function() "data1",
        filename_2 = function() "data2",
        rdf_ref = function(rv, input_field) rdf,
        rdf_comp = function(rv, input_field) rdf,
        ddf_ref = function(rv, input_field) ddf,
        ddf_comp = function(rv, input_field) ddf,
        ddf_cols_ref = function(rv, input_field) colnames(ddf),
        ddf_cols_comp = function(rv, input_field) colnames(ddf),
        ddf_condcol_ref = function(rv, input_field) "group",
        ddf_condcol_comp = function(rv, input_field) "group",
        ddf_samplecol_ref = function(rv, input_field) "sample",
        ddf_samplecol_comp = function(rv, input_field) "sample",
        samples = function(rv, input_field, prefix = "") paste(prefix, ddf$sample, sep = ""),
        design_condcol_1 = function() "group",
        design_condcol_2 = function() "group",
        figure_save_format = function() "png",
        figure_save_width = function() 800,
        figure_save_height = function() 600
    )
}

test_that("PCA module renders plotly outputs for valid inputs", {
    skip_if_not_installed("shiny")

    set.seed(123)
    rdf <- data.frame(
        feature = paste0("F", 1:50),
        s1 = rnorm(50),
        s2 = rnorm(50),
        s3 = rnorm(50),
        s4 = rnorm(50),
        stringsAsFactors = FALSE
    )
    ddf <- data.frame(
        sample = c("s1", "s2", "s3", "s4"),
        group = c("A", "A", "B", "B"),
        stringsAsFactors = FALSE
    )

    rv <- make_mock_pca_rv(rdf, ddf)

    shiny::testServer(
        module_pca_server,
        args = list(rv = rv, module_name = "PCA"),
        {
            suppressWarnings(session$setInputs(
                dataset1 = "data1",
                dataset2 = "data2",
                pairplot = FALSE,
                pairplot_pcs = 2,
                pc_comp_1_data1 = 1,
                pc_comp_2_data1 = 2,
                color_data1 = "group",
                data1_as_factor = TRUE,
                shape_data1 = "None",
                do_filter_samples_data1 = FALSE,
                filter_cond_data1 = "None",
                display_levels_data1 = c("A", "B"),
                pc_comp_1_data2 = 1,
                pc_comp_2_data2 = 2,
                color_data2 = "group",
                data2_as_factor = TRUE,
                shape_data2 = "None",
                do_filter_samples_data2 = FALSE,
                filter_cond_data2 = "None",
                display_levels_data2 = c("A", "B"),
                dot_size = 3,
                scale_pca_data = TRUE,
                center_pca_data = TRUE,
                show_labels_data = FALSE,
                show_loadings = FALSE,
                variance_filter_data = 0.1,
                custom_title1 = "",
                custom_title2 = "",
                text_size = 10
            ))
            suppressWarnings(session$flushReact())

            expect_s3_class(output$pca_plot1, "json")
            expect_s3_class(output$pca_plot2, "json")
        }
    )
})
