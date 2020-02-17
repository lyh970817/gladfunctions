# gladfunctions

A collection of R functions to clean, export, summarize and plot GLAD (*Genetic Links to Depression and Anxiety*) questionnaire data.


## How to Install

Requires the `devtools` package:

```r
devtools::install_github("lyh970817/gladfunctions")
```

## Overview

To see the full list of exported functions:

```{r}
library("tinyutils")
ls("package:gladfunctions")
```

Or check out the vignette [here](https://htmlpreview.github.io/?https://github.com/lyh970817/gladfunctions/blob/master/vignettes/gladfunctions_vignette.html)

Requires the raw Qualtrics exports or the cleaned data files exported by the functions. Please speak to Henry Rogers 
in the BioResource office for a `python` script to extract exports from Qualtrics.

## Issues

Please open an issue on this GitHub page to report bugs and suggest changes.
