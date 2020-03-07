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

# New

* `GLAD_getdescr` to get descriptions for variables (without needing to look at the dictionary manually)

* `feather` format exports

* Vectorized `GLAD_sheet` function

* Some more complicated derived variable examples on AGP and on ED

* Put negative to positive for all continuous variables in recoding

* Specifying limits for plots - useful for `include_outlier` examination

* Using text files named after questionnaire names containing variable lists to select variables

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

# To-do

* Gender difference plots for all the continuous variables

