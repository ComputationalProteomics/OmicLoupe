# OmicLoupe

Understanding expression across comparisons and datasets

![OmicLoupe illustration](man/figures/overview_visualization.png)

## Dependencies

The R packages used by OmicLoupe have certain system dependences. If working on a Debian-based Linux distribution, these can be installed from the command line:

```
sudo apt install libcurl4-openssl-dev libxml2-dev libssl-dev
```

## Installation

It can be installed either directly from GitHub or by downloading as a Zip, by running the following commands from an R console:

```{r}
devtools::install_github("ComputationalProteomics/OmicLoupe")
```

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

## Running it as a Singularity container

Singularity is a container software which (similar to Docker) allows execution without needing to prepare local dependencies. It can be downloaded from https://singularity.lbl.gov.

Using Singularity can be a rapid way to get OmicLoupe running locally without needing to install all dependencies. First, retrieve the container:

```{bash}
singularity pull --name OmicLoupe.simg shub://ComputationalProteomics/OmicLoupe
```

This will download the Singularity container containing OmicLoupe to `OmicLoupe.simg`. Now you are ready to run OmicLoupe:

```{bash}
singularity run OmicLoupe.simg
```

Simply open the provided link in a browser, and you should have access to OmicLoupe.

## Running on a server

If you have a server running [Shiny Server](https://rstudio.com/products/shiny/shiny-server/) you can easily get OmicLoupe running by:

1. Install OmicLoupe (making sure it get installed at a path which Shiny Server can access)
2. Place a file called `app.R` within a folder in the `shiny-server` directory containing a single line calling OmicLoupe: `OmicLoupe::runApp()`
