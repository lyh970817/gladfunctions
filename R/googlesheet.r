GLAD_url <- "https://docs.google.com/spreadsheets/d/1X9LfgPpBsS2zzVSwkncX8aP5YWuEH9PdW11KRDRcZ38/edit#gid=1962975491"

GLAD_sheet_scal <- function(questionnaire) {
  # Scalar version
  googlesheet <- read_sheet(GLAD_url,
    sheet = questionnaire,
    col_types = "c", skip = 1
  ) %>%
    rename(
      easyname = Easy.name,
      oldvar = Qualtrics.variable,
      newvar = New.variable,
      title = Description,
      levels = Levels,
      labels = Labels,
      type = Type
    ) %>%
    (function(x) {
      if ("GLAD.t0" %in% colnames(x)) {
        x <- rename(x, GLAD.t0 = GLAD.t0)
      } else {
        message("Score.key is not in ", questionnaire)
      }
      if ("Score.key" %in% colnames(x)) {
        x <- rename(x, score_key = Score.key)
      } else {
        message("Score.key is not in ", questionnaire)
      }
      if ("Subscale" %in% colnames(x)) {
        x <- rename(x, subscale = Subscale)
      } else {
        message("Subscale is not in ", questionnaire)
      }
      if ("Minimum" %in% colnames(x)) {
        x <- rename(x, min = Minimum)
      } else {
        message("Minimum is not in ", questionnaire)
      }
      if ("Maximum" %in% colnames(x)) {
        x <- rename(x, max = Maximum)
      } else {
        message("Maximum is not in ", questionnaire)
      }
      if ("Unit" %in% colnames(x)) {
        x <- rename(x, unit = Unit)
      } else {
        message("Unit is not in ", questionnaire)
      }
      if ("Formula" %in% colnames(x)) {
        x <- rename(x, formula = Formula)
      } else {
        message("Formula is not in ", questionnaire)
      }
      return(x)
    })
  return(googlesheet)
}

#' Read in The Dictionary Sheet
#'
#' Read in the googlesheet dictionary sheet for a specified questionnaire
#'
#' @param questionnaire The acronym of the questionnaire as one of the
#' sheet names in the GLAD dictionary googleshet.
#' @return A dataframe containing information of the dictionary sheet.
#' @export
GLAD_sheet <- Vectorize(GLAD_sheet_scal, SIMPLIFY = FALSE)
