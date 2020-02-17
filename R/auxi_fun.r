#' @import dplyr
#' @import readr
#' @import googlesheets4
#' @import tidyr
#' @import purrr
#' @import tibble
#' @import stringr
#' @import summarytools
#' @import kableExtra
#' @import ggformula
#' @import ggrepel
#' @import psych
#' @import lubridate
#' @import car

sheet_extract <- function(col, var, googlesheet) {
  # Extract values for a specified variable from a specified column

  easynames <- googlesheet[["easyname"]]
  newvars <- googlesheet[["newvar"]]

  if (var %in% newvars) {
    value <- googlesheet[[col]][which(googlesheet[["newvar"]] == var)]
  } else if (var %in% easynames) {
    value <- googlesheet[[col]][which(googlesheet[["easyname"]] == var)]
  } else {
    stop(var, " does not exist in the dictionary.")
  }

  if (col %in% c("unit", "type", "title", "min", "max")) {
    value <- value %>%
      unique() %>%
      na.omit()
    if (col %in% c("min", "max")) {
      value <- as.numeric(value)
    }
  }
  return(value)
}

is_rowna <- function(data) {
  apply(data, 1, function(x) {
    all(is.na(x))
  })
}

get_categvars <- function(var, googlesheet) {
  # Given one of the categorical variables that allow multiple options, get
  # all the others that belong to the same question on Qualtircs.
  easynames <- googlesheet[["easyname"]]
  newvars <- googlesheet[["newvar"]]

  oldvar <- sheet_extract("oldvar", var, googlesheet)
  oldvar_no_unscore <- str_split(oldvar, "_")[[1]][[1]]
  var_is <- grep(oldvar_no_unscore, googlesheet[["oldvar"]])
  if (var %in% newvars) {
    vars <- unique(googlesheet[["newvar"]][var_is])
  } else if (var %in% easynames) {
    vars <- unique(googlesheet[["easyname"]][var_is])
  }
  return(vars)
}

GLAD_select <- function(data, which, googlesheet) {
  # Selection is based on googlesheet index (formula is which - 2)
  # I thought this might be the most easy.

  if (any(colnames(data)) %in% googlesheet[["newvar"]]) {
    return(data[googlesheet[["newvar"]][which - 2]])
  }
  if (any(colnames(data)) %in% googlesheet[["easyname"]]) {
    return(data[googlesheet[["easyname"]][which - 2]])
  }
}
