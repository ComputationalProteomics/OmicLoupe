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

help_setup_setup <- sprintf("Expected workflow: %s", parse_vector_to_bullets(
    c(
        "Upload one or more dataset files together with respective design files (further instructions on file formats under 'Help')", 
        "Assign sample columns in the dataset file(s), normally using the 'Detect columns' button", 
        "Assign statistical columns, normally using the 'Detect columns' button. Note that these should follow a format outlined under 'Help'",
        "Finally, if more than one dataset - perform mapping which will match feature columns from the two datasets",
        "If running matched samples click the 'Matched samples' button and use only one design matrix, this will unlock the 'Correlation' tab"
    )
))

help_table_setup <- c(
    "
    Gives an overview of the uploaded datasets as well as the mapped dataset. Allows formatting the table display which is displayed
    in different sections of OmicLoupe. Also allows adjusting the stat pattern suffixes looked for when running 'Identify columns'.
    "
)

help_quality <- c(
    "
    Provides quality inspections with the option to highlight colorings from the design matrix.
    The boxplots, density and barplots illustrations provide dataset wide illustrations (further discussed under Help).
    The histograms allows studying the distribution of any feature values, and additionally allows cross-coloring with conditions.
    "
)

help_pca <- c(
    "
    Generate principal component analysis comparisons. Allows comparing either multiple conditions
    for the same dataset, or comparing across two separate datasets.
    "
)

help_spotcheck <- c(
    "
    Allows spot-checking the distribution of any singular feature, across a condition
    of interest obtained from the design matrix. The target feature can be selected from
    any mapped table illustration and is illustrated by switching to this tab.
    "
)

help_statistics <- c(
    "
    Requires at least two statistical comparisons to be present.
    <br><br>
    Study feature distribution using Volcano and MA scatter plots, as well as
    across P-value histograms. This allows revealing how features are distributed both
    simultaneously across these viewing angles, but also in other contrasts or datasets.
    Subsets of the data can be marked to study the features directly in a table, and to
    see their distributions in the other comparison. The dataset can also be colored
    using principal component loadings to relate them to the patterns studied there,
    and by any arbitrary feature annotation.
    "
)

help_overlap <- c(
    "
    Requires at least two statistical comparisons to be present.
    <br><br>
    Compare overlap of features passing statistical thresholds (FDR or P-value).
    For <b>Venn</b>, two contrasts are compared. Here, the selected subset of features can
    be studied in the table below. For <b>Upset</b> the overlap across an arbitrary
    number of comparisons can be studied.
    "
)

help_correlation <- c(
    "
    Only available when running two datasets with 'Matched samples'.
    <br><br>
    Displays correlation histograms of intensity values present in the two datasets.
    "
)