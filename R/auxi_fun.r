#' @importFrom magrittr %>%
#' @import ggplot2
#' @import knitr
#' @importFrom kableExtra kable_styling scroll_box
#' @importFrom purrr map_chr map_df map2_df map_dbl imap_dfc map_lgl map2_lgl
#' @importFrom dplyr select group_by filter bind_cols count rename mutate
#' left_join nth everything contains summarise
#' @importFrom stringr str_extract str_split
#' @importFrom tidyr gather
#' @importFrom ggrepel geom_text_repel
#' @importFrom psych scoreItems
#' @importFrom stats fligner.test bartlett.test
#' @importFrom car leveneTest
#' @importFrom readr write_csv read_csv
#' @importFrom googlesheets4 read_sheet sheets_sheets
#' @importFrom summarytools descr tb freq
#' @import ggformula
#' @import lfactors

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

#' Exports Selected Variables From a Data Set
#'
#' Exports selected variables from a data set by specifying their names.
#'
#' @param data The dataframe containing the variables to be exported,
#' outputted by 'GLAD_clean' or 'GLAD_cleanall'.
#' @param which An character vector indicating the items to be export.
#' @param googlesheet A dataframe created by 'GLAD_sheet' that contains
#' corresponding dictionary information for 'data'.
#' @export
GLAD_select <- function(data, which, googlesheet) {
  if (any(colnames(data) %in% googlesheet[["newvar"]])) {
    items <- googlesheet[["newvar"]][googlesheet[["easyname"]] %in% which]
    items_num <- paste(items, "numeric", sep = ".")
    items_all <- c(items, items_num)
    return(bind_cols(
      data[c("ID", "Sex", "Age", "Birthyear")],
      data[colnames(data) %in% items_all]
    ))
  }
  if (any(colnames(data) %in% googlesheet[["easyname"]])) {
    items_num <- paste(which, "numeric", sep = ".")
    items_all <- c(which, items_num)
    return(bind_cols(
      data[c("ID", "Sex", "Age", "Birthyear")],
      data[colnames(data) %in% items_all]
    ))
  }
}
