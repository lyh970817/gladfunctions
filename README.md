# gladfunctions

A collection of R functions to clean, export, summarise and plot questionnaire data of the GLAD (*Genetic Links to Depression and Anxiety*) study.


# How to Install

Requires the `devtools` package:

```r
devtools::install_github("lyh970817/gladfunctions", upgrade_dependencies = FALSE)
```

# Overview

To see the full list of exported functions:

```{r}
library(gladfunctions)
ls("package:gladfunctions")
```

Or check out the `rmarkdown` vignette [here](https://htmlpreview.github.io/?https://github.com/lyh970817/gladfunctions/blob/master/vignettes/gladfunctions_vignette.html)

Requires raw Qualtrics exports or cleaned data files exported by the functions. 

Please speak to **Henry Rogers** in the **BioResource Office** for a `python` script to prepare Qulatrics exports (extract exports from Qualtrics and remove participant personal information).

# Issues

Please open an issue on this GitHub page to report bugs and suggest changes.
