questionnaires <- c(
  "PHY", "CIDID", "DEM", "MHD", "PHQ", "MSM", "MDQ",
  "GAD", "CIDIA", "AUDIT", "SUB", "CIDIP", "CTS", "ATS",
  "PCL", "SWB", "SPEC", "SOCP", "PAD", "AGP", "SASPD", "WSAS",
  "UXP", "CAM", "NHS", "SPOVI", "EPDS", "GAM",
  "LIFE",
  "FAM", "ED", "NES", "MDDI", "OCIR", "DCQ", "DRUG", "MIG",
  "PTSD", "FEAR", "CARE", "SLEEP"
)

sign_up <- c(
  "DEM", "PHY", "MHD", "PHQ", "CIDID", "MSM", "MDQ", "GAD", "CIDIA", "AUDIT", "SUB", "CIDIP", "CTS",
  "ATS", "PCL", "SWB", "SPEC", "SOCP", "PAD", "SASPD", "AGP", "WSAS", "UXP",
  "CAM", "NHS"
)

GLAD_removedup <- function(data) {
  # Remove participants with duplicated IDs.

  data["dup"] <- duplicated(data["ExternalReference"])
  # print(summary(data["dup"]))
  data <- data %>%
    filter(dup == FALSE) %>%
    select(-dup)
  return(data)
}

GLAD_removehead <- function(data, googlesheet) {

  # Remove variables containg "HEAD" in the file.
  # These are variables not displayed to participants but are exported by
  # Qualtircs.

  head_vars <- googlesheet[["oldvar"]][grep("HEAD", googlesheet[["newvar"]])]
  return(data[!colnames(data) %in% head_vars])
}
# These two above can be put in the python script

select_vars <- function(questionnaire, data_raw, sheet) {

  # "oldvar" are variables names Qualtrics raw files have.
  # They correponds to column names of a data set.
  sheet_vars <- sheet[["oldvar"]]

  if ("GLAD.t0" %in% colnames(sheet)) {
    in_glad <- !is.na(sheet[["GLAD.t0"]])
  } else {
    in_glad <- TRUE
  }

  # What variables are in the dictionay but not in the data frame but have
  # GLAD.t0
  morevars <- sheet_vars[which((!sheet_vars %in% colnames(data_raw)) & in_glad)] %>%
    unique()

  if (length(morevars[!is.na(morevars)]) > 0) {
    # One per row
    morevars_str <- paste(morevars, collapse = ", ")
    message(paste(
      morevars_str,
      "are in the dictionary but not in the dataframe;",
      "\n(Please ignore this for dictionary editing. It's a Qualtrics related error)"
    ))
    # The NAs in morevars are most likely empty rows in the dictionary.
    if (any(is.na(morevars))) {
      empty_row_str <- paste(which(is.na(sheet_vars)) + 2, collapse = ", ")
      message(paste("There are empty Qualtrics.variable at row", empty_row_str))
    }
  }

  # Find variables that have Qulatrics.derived.variables and Derived.variables
  # in "Comments"
  not_derived <- !grepl("[Dd]erived", sheet[["Comments"]])
  # Select only variables that are in the dataframe.
  vars <- sheet_vars[which(sheet_vars %in% colnames(data_raw) & not_derived & in_glad)]
  return(vars)
}

questionnaire_clean <- function(questionnaire, data_raw, path, limits, rename, format) {
  sheet <- GLAD_sheet(questionnaire)[[1]]
  vars <- select_vars(questionnaire, data_raw, sheet)
  data_raw <- data_raw %>%
    mutate(
      Sex = lfactor(DEM.SEX.1.0, levels = c(0, 1), labels = c("Male", "Female")),
      Age = DEM.AGE.1.0,
      # The birthyear exported by Qualtrics has two-digit format.
      Birthyear = DEM.DOB.3.0 + 1900
    ) %>%
    select(
      ExternalReference,
      Sex,
      Age,
      Birthyear,
      vars
    ) %>%
    filter(!is_rowna(.[5:ncol(.)])) %>%
    GLAD_removedup() %>%
    GLAD_removehead(sheet) %>%
    .[complete.cases(.[["Sex"]]), ]

  data_cleaned <- data_raw %>%
    GLAD_recode_df(googlesheet = sheet, limits = TRUE)

  GLAD_export(data_cleaned, data_raw,
    questionnaire = questionnaire,
    dirpath = path, googlesheet = sheet,
    format, rename
  )
}

#' Cleans Questionnaires
#'
#' Cleans all questionnaires or one specified questionnaire in 'dat_list'
#' and creates exports.
#'
#' The cleaning process removes Qualtrics derived variables, removes
#' participants with duplicated IDs or without gender information, recodes
#' `Categorical` and `Binary` variables to class 'lfactor' from the
#' 'lfactor' package with labels attached and create numeric copies,
#' applies limits to `Numeric/Continuous` variables and creates four
#' version of export files: RDS and CSV files with `New.variable` names,
#' and RDS and CSV files with `Easy.name` names.
#'
#' @param questionnaire A character string indicating what questionnaire to
#' clean by its acronym. The questionnaire data must be in 'dat_list'.If
#' it's "All", all questionnaires in 'dat_list' are cleaned.
#' @param dat_list A named list of dataframes produced by 'GLAD_read'.
#' @param limits A logical indicating whether limits (min and max) are to
#' be applied
#' @param rename A logical. TRUE if the variables are to be renamed to
#' `Easy.name`.
#' @param format A character string. It should be one of c("feather",
#' "rds", "sav", "dta", "sas")
#' @export
GLAD_clean <- function(questionnaire, dat_list, path, limits = TRUE, rename = TRUE, format = "feather") {
  if (length(dat_list) == 0) {
    stop("You have not read in the data files.")
  }

  # We always need "DEM" to extract "Sex", "Age" and "Birthyear"
  dem <- dat_list[["DEM"]]

  if (questionnaire %in% sign_up) {
    # If the questionnaire is in sign-up hence in "DEM",
    # we already have "Sex", "Age" and "Birthyear" in the same file.
    try(questionnaire_clean(questionnaire, dem, path, limits, rename, format))
  } else if (questionnaire %in% c("NES", "MDDI")) {
    try(dat_list[["ED"]] %>%
      left_join(dem[
        c(
          "ExternalReference",
          "DEM.SEX.1.0",
          "DEM.AGE.1.0",
          "DEM.DOB.3.0"
        )
      ],
      by = "ExternalReference"
      ) %>%
      questionnaire_clean(questionnaire, ., path, limits, rename, format))
  }
  else if (questionnaire %in% names(dat_list)) {
    # if the questionnaire is not in sign-up we need to merge it with
    # those variables in "DEM".
    try(dat_list[[questionnaire]] %>%
      left_join(dem[
        c(
          "ExternalReference",
          "DEM.SEX.1.0",
          "DEM.AGE.1.0",
          "DEM.DOB.3.0"
        )
      ],
      by = "ExternalReference"
      ) %>%
      questionnaire_clean(questionnaire, ., path, limits, rename, format))
  } else if (questionnaire == "All") {
    all_names <- c("DEM", setdiff(questionnaires, sign_up))
    if (all_names == names(dat_list)) {
      stop("You have not read in data for all questionnaires")
    }
    GLAD_cleanall(dat_list, path, limits, rename, format)
  }
}

GLAD_cleanall <- function(dat_list, path, limits, rename, format) {
  for (q in questionnaires) {
    GLAD_clean(q, dat_list, path, limits, rename, format)
  }
}
