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
#' @importFrom feather write_feather read_feather
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

get_selected <- function(data, which) {
  if (length(which) == 0) {
    return(data)
  }

  items_num <- paste(which, "numeric", sep = "_")
  items_all <- c(which, items_num)

  return(bind_cols(
    data[c("ID", "sex", "age", "birthyear", "startdate", "enddate")],
    data[colnames(data) %in% items_all]
  ))
}

#' Read in Selected Variables From Data sets to a list
#'
#' Read in selected variables specified through *.txt files from data sets
#' to a list
#'
#' @param clean_path Path to the folder containing the  exported files.
#' @param export_path Path to the data request folder, which should contain
#' subfolders each represent a person who requests the data
#' @param person The name of the person who requests the data
#' @param which A character vector specifying paths to the *.txt files
#' containing required variables. Each text file should correspond to and
#' has the name of a questionnaire. The variables within each text file
#' should be on seperate lines.
#' @param format Format of exported data. It should be one of c('rds',
#' 'sav', 'sas', 'feather', 'csv')
#' @export
GLAD_select <- function(clean_path, export_path, person, which, format) {
  if (length(format) > 1) {
    stop("Only one format allowed.")
  }
  if (!format %in% c("rds", "sav", "sas", "feather", "csv")) {
    stop("format must be one of c('rds', 'sav', 'sas', 'feather', 'csv')")
  }
  var_list <- readLines(which)
  questionnaires <- str_split(var_list, "\\.|_") %>%
    map_chr(nth, 1) %>%
    unique() %>%
    toupper()

  sheets <- GLAD_sheet(questionnaires)

  vars_eachq <- map(questionnaires, function(name) {
    grep(paste0(tolower(name), "[\\.|_]"), var_list, value = TRUE) %>%
      return()
  }) %>%
    setNames(questionnaires)

  select_list <- map2(questionnaires, sheets, function(q, s) {
    vars <- vars_eachq[[q]]
    dat <- readRDS(paste0(clean_path, "rds_renamed/", paste0(q, "_Renamed.rds")))
    dat_derived <- GLAD_derive(dat, s)
    derived_vars <- setdiff(names(dat_derived), names(dat))

    if (format == "feather") dat <- read_feather(paste0(clean_path, "feather_renamed/", paste0(q, "_Renamed.feather")))
    if (format == "dta") dat <- read_dta(paste0(clean_path, "dta_renamed/", paste0(q, "_Renamed.dta")))
    if (format == "sav") dat <- read_sav(paste0(clean_path, "sav_renamed/", paste0(q, "_Renamed.sav")))
    if (format == "sas") dat <- read_sas(paste0(clean_path, "sas_renamed/", paste0(q, "_Renamed.sas")))

    dat <- bind_cols(dat, dat_derived[derived_vars])

    return(get_selected(dat, vars))
  }) %>% setNames(questionnaires)

  time <- format(Sys.time(), "%m-%d-%Y")
  dir <- paste0(export_path, person, "-", time, "/")
  dir.create(dir, showWarnings = FALSE)

  dir_text <- paste0(dir, "variables_selected/")
  dir.create(dir_text, showWarnings = FALSE)

  file.copy(which, dir_text)

  for (i in seq_along(select_list)) {
    if (format == "rds") saveRDS(select_list[[i]], paste0(dir, questionnaires[i], ".rds"))
    if (format == "feather") write_feather(select_list[[i]], paste0(dir, questionnaires[i], ".feather"))
    if (format == "dta") write_dta(select_list[[i]], paste0(dir, uestionnaires[i], ".dta"))
    if (format == "sav") write_sav(select_list[[i]], paste0(dir, questionnaires[i], ".sav"))
    if (format == "sas") write_sas(select_list[[i]], paste0(dir, questionnaires[i], ".sas"))
  }
}
