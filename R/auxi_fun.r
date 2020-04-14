#' @importFrom magrittr %>%
#' @import ggplot2
#' @import knitr
#' @importFrom kableExtra kable_styling scroll_box
#' @importFrom purrr map map_chr map_df map2_df map2 map_dbl imap_dfc map_lgl map2_lgl
#' @importFrom dplyr select group_by filter bind_cols count rename mutate
#' left_join nth everything contains summarise
#' @importFrom stringr str_extract str_split boundary
#' @importFrom tidyr gather
#' @importFrom ggrepel geom_text_repel
#' @importFrom psych scoreItems
#' @importFrom stats fligner.test bartlett.test
#' @importFrom car leveneTest
#' @importFrom haven labelled write_sav write_sas write_dta read_sav
#' read_sas read_dta
#' @importFrom readr write_csv read_csv
#' @importFrom googlesheets4 read_sheet sheets_sheets
#' @importFrom summarytools descr tb freq
#' @importFrom feather write_feather feather
#' @import ggformula
#' @import lfactors

palette <- c("#efc00b", "#b7dee8")

GLAD_sheetnames <- function(url = GLAD_url) {
  GLAD_sheetnames <- sheets_sheets(url) %>%
    .[grep("Overview|Tracker|Updates|Cut offs|GLAD.complete|Information",
      .,
      invert = T
    )]

  return(GLAD_sheetnames)
}

sheet_extract <- function(col, var, googlesheet) {
  # Extract values for a specified variable from a specified column

  easynames <- googlesheet[["easyname"]]
  newvars <- googlesheet[["newvar"]]

  if (var %in% newvars) {
    value <- googlesheet[[col]][which(newvars == var)]
  } else if (var %in% easynames) {
    value <- googlesheet[[col]][which(easynames == var)]
  } else {
    stop(var, " does not exist in the dictionary.")
  }

  if (col %in% c("unit", "type", "title", "min", "max")) {
    value <- value %>%
      unique() %>%
      .[!is.na(.)]
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

get_questionnaire <- function(googlesheet) {
  # This is to be changed. Use parent frame to emulate dynamic scoping Get
  # the name of the questionnaire
  questionnaire <- str_split(
    googlesheet[["newvar"]],
    "\\."
  ) %>%
    map_chr(nth, 1) %>%
    unique() %>%
    .[!is.na(.)]
  return(questionnaire)
}

GLAD_getdescr_scal <- function(which, googlesheet) {
  # scalar version
  return(sheet_extract("title", which, googlesheet))
}

#' Get Descripton (Title) for Selected Variables
#'
#' Get Descripton (Title) for Selected Variables by specifying their names.
#'
#' @param which An character vector indicating the items.
#' @param googlesheet A dataframe created by 'GLAD_sheet' that contains
#' corresponding dictionary information for 'data'.
#' @return A named vector with names being the `Easy.name` or
#' `New.variable` and values being their descriptions.
#' @export
GLAD_getdescr <- Vectorize(GLAD_getdescr_scal, vectorize.args = "which")

