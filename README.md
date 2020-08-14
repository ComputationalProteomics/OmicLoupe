# OmicLoupe
Understanding expression across comparisons and datasets

## Installation

For now, it can be installed by downloading as a Zip, and subsequently execute the following from within an R console:

```{r}
devtools::install_local("OmicLoupe-master.zip")
```

## Running it locally

After installation, you can immediately run the program.
You can subsequently navigate to the browser to access the software.

```{r}
> OmicLoupe::runApp()
```

You could make a convenient Bash alias for this, which lets you execute OmicLoupe by simply typing "omicloupe" into a Bash terminal.
Add this line to your .bash_aliases or .bashrc file.

```{r}
alias omicloupe="Rscript -e \"runApp()\""
```

## Running on a server

If you have a server running [Shiny Server](https://rstudio.com/products/shiny/shiny-server/) you can easily get OmicLoupe running by:

1. Install OmicLoupe (making sure it get installed at a path which Shiny Server can access)
2. Place a file called `app.R` within a folder in the `shiny-server` directory containing a single line calling OmicLoupe: `OmicLoupe::runApp()`
