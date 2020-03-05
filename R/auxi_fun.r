#' @importFrom magrittr %>%
#' @import ggplot2
#' @import knitr
#' @importFrom kableExtra kable_styling scroll_box
#' @importFrom purrr map_chr map_df map2_df map2 map_dbl imap_dfc map_lgl map2_lgl
#' @importFrom dplyr select group_by filter bind_cols count rename mutate
#' left_join nth everything contains summarise
#' @importFrom stringr str_extract str_split boundary
#' @importFrom tidyr gather
#' @importFrom ggrepel geom_text_repel
#' @importFrom psych scoreItems
#' @importFrom stats fligner.test bartlett.test
#' @importFrom car leveneTest
#' @importFrom readr write_csv read_csv
#' @importFrom googlesheets4 read_sheet sheets_sheets
#' @importFrom summarytools descr tb freq
#' @importFrom feather write_feather
#' @import ggformula
#' @import lfactors

palette <- c("#efc00b", "#b7dee8")

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

get_selected <- function(data, which, googlesheet) {
  if (any(colnames(data) %in% googlesheet[["newvar"]])) {
    # The specified names (which) are always easy names.
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

#' Exports Selected Variables From a Data Set
#'
#' Exports selected variables specified through a *.txt file
#'
#' @param clean_path Path to where the exported files are.
#' @param which Path to the *.txt file containing required variables. Each
#' text file should correspond to and has the name of a questionnaire. The
#' variables within each text file should be on seperate lines.
#' @export
GLAD_select <- function(clean_path, which) {
  questionnaires <- str_split(which, boundary("word")) %>%
    map_chr(nth, -1) %>%
    str_extract("[A-Z]*")
  sheets <- GLAD_sheet(questionnaires)
  names(which) <- questionnaires
  select_list <- map2(questionnaires, sheets, function(q, s) {
    vars <- readLines(which[q])
    dat <- readRDS(file.path(clean_path, "rds_renamed", paste0(q, "_Renamed.rds")))
    selected_dat <- get_selected(dat, vars, s)
  }) %>% setNames(questionnaires)
  return(select_list)
}

get_questionnaire <- function(googlesheet) {
  # Get the name of the questionnaire
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
