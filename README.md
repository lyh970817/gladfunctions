<p align="center">
  <img src="https://github.com/lyh970817/gladfunctions/blob/master/glad_logo.jpg" width="350" class="center">
</p>

# gladfunctions

A collection of R functions to clean, export, summarise and plot questionnaire data of the GLAD (Genetic Links to Anxiety and Depression) study.


# How to Install

Requires the `devtools` package:

```r
devtools::install_github("lyh970817/gladfunctions")
```

# Overview

To see the full list of exported functions:

```{r}
library(gladfunctions)
ls("package:gladfunctions")
```

Or check out the Rmarkdown [vignette](https://htmlpreview.github.io/?https://github.com/lyh970817/gladfunctions/blob/master/vignettes/gladfunctions_vignette.html)
and the [source code](https://github.com/lyh970817/gladfunctions/blob/master/vignettes/gladfunctions_vignette.Rmd).

Requires raw Qualtrics exports or cleaned data files exported by the functions.

Please speak to **Henry Rogers** in the **BioResource Office** for a `python` script to prepare Qulatrics exports (extract exports from Qualtrics and remove participant personal information).

# Issues

Please open an issue on this GitHub page to report bugs and suggest changes.
