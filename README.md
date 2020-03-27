# OmicLoupe
Understanding expression across comparisons and datasets

## Installation

For now, it can be installed by downloading as a Zip, and subsequently execute the following from within an R console:

```{r}
devtools::install_local("OmicLoupe-master.zip")
```

## Running it locally

You can easily execute OmicLoupe locally by navigating to the source directory and executing the following bash command. 
You can subsequently navigate to the browser to access the software.

```{r}
> source('app.R')
> runApp()
```

You could make a convenient Bash alias for this, which lets you execute OmicLoupe by simply typing "omicloupe" into a Bash terminal.
Add this line to your .bash_aliases or .bashrc file.

```{r}
alias omicloupe="cd ~/src/OmicLoupe; Rscript -e \"source('app.R');runApp()\""
```
