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

questionnaire_clean <- function(questionnaire, data_raw) {
  sheet <- GLAD_sheet(questionnaire)

  # "oldvar" are variables names Qualtrics raw files have.
  # They correponds to column names of a data set.
  sheet_vars <- sheet[["oldvar"]]

  # What variables are in the dictionay but not in the data frame?
  morevars <- sheet_vars[which(!sheet_vars %in% colnames(data_raw))] %>%
    unique()
  if (length(morevars[!is.na(morevars)]) > 0) {
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

  # Select only variables that are in the dictionary sheet and are not
  # derived variabels to extract.  Variables that are only in the data are
  # mostly not useful, they could be heading variables exported by
  # Qualtrics.
  vars <- sheet_vars[which(sheet_vars %in% colnames(data_raw) & not_derived)]

  data_raw <- data_raw %>%
    mutate(
      Sex = factor(DEM.SEX.1.0, levels = c(0, 1), labels = c("Male", "Female")),
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
    filter(!is_rowna(.)) %>%
    GLAD_removedup() %>%
    GLAD_removehead(sheet) %>%
    .[complete.cases(.[["Sex"]]), ]

  data_cleaned <- data_raw %>%
    GLAD_recode_df(googlesheet = sheet)

  # This exports New.variable name files.
  GLAD_export(data_cleaned, data_raw,
    questionnaire = questionnaire,
    dirpath = clean_path, googlesheet = sheet,
    rename = FALSE
  )

  # This exports Easy.name files.
  GLAD_export(data_cleaned, data_raw,
    questionnaire = questionnaire,
    dirpath = clean_path, googlesheet = sheet,
    rename = TRUE
  )
}

#' Cleans One Specified Questionnaire
#'
#' Cleans the specified questionnaire and creates export.
#'
#' @param questionnaire A questionnaire that is to be cleaned.
#' @param dat_list A named list of dataframes produced by 'GLAD_rawall'.
#' @export
GLAD_clean <- function(questionnaire, dat_list) {
  # We always need "DEM" to extract "Sex", "Age" and "Birthyear"
  dem <- dat_list[["DEM"]]
  if (questionnaire %in% sign_up) {
    # If the questionnaire is in sign-up hence in "DEM",
    # we already have "Sex", "Age" and "Birthyear" in the same file.
    try(questionnaire_clean(questionnaire, dem))
  } else if (questionnaire %in% names(dat_list)) {
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
      questionnaire_clean(questionnaire, .))
  }
}

#' Cleans All Questionnaires In a List
#'
#' Cleans all the questionnaires in a list produced by 'GLAD_rawall'
#' and creates exports.
#'
#' @param dat_list A named list of dataframes produced by 'GLAD_rawall'.
#' @export
GLAD_cleanall <- function(dat_list) {
  for (q in questionnaires) {
    GLAD_clean(q, dat_list)
  }
}
