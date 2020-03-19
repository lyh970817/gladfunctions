get_keys <- function(items, googlesheet) {
  map_dbl(items, function(x) {
    googlesheet[["score_key"]][which(googlesheet[["easyname"]] == x)][1] %>%
      as.numeric()
  })
}

get_score <- function(keys, data) {
  # We can freely `as.numeric` columns here as all the factor variables are
  # of `lfactor` class, which allows `as.numeric`
  data <- map_df(data, as.numeric)
  scores <- scoreItems(
    keys = keys,
    items = data,
    totals = TRUE
  )
  # `scoreItems` also computes scores for participants with missing items.
  # We will discard these score.
  scores[["scores"]][scores$missing > 0] <- NA
  return(scores[["scores"]][, 1])
}

GLAD_score <- function(data, googlesheet, questionnaire) {
  is_newvar <- any(colnames(data) %in% googlesheet[["newvar"]])
  if (is_newvar) {
    # The formulae are in Easy.name, so if the data frame has New.variable
    # names it should be renamed first.
    data <- GLAD_rename(data, googlesheet,
      from = "newvar",
      to = "easyname"
    )
  }

  vars <- googlesheet[["easyname"]]
  formulae <- googlesheet[["formula"]]

  if (!"score_key" %in% colnames(googlesheet)) {
    # message(paste(questionnaire, "has no Score.key."))
    return(data)
  }
  keys_pos <- which(!is.na(googlesheet[["score_key"]]))

  browser()
  if (length(keys_pos) == 0) {
    message(paste(questionnaire, "has no Score.key."))
    return(data)
  }

  items <- vars[keys_pos] %>% unique()
  data_items <- data[items]
  all_keys <- get_keys(items, googlesheet)

  if (length(all_keys) >= 1) {
    if (!any(formulae == questionnaire, na.rm = T)) {
      message(questionnaire, " has no total score formula.")
    } else {
      total_score_name <- vars[which(formulae == questionnaire)]
      data[total_score_name] <- get_score(all_keys, data_items)
    }
  }

  subscales <- unique(googlesheet[["subscale"]]) %>%
    .[!is.na(.)]

  if (length(subscales > 1)) {
    for (subscale in subscales) {
      sub_items <-
        vars[which(googlesheet[["subscale"]] == subscale)] %>%
        unique()
      data_subitems <- data_items[sub_items]
      sub_keys <- get_keys(sub_items, googlesheet)
      sub_score_name <- vars[which(googlesheet[["formula"]] == subscale)]
      data[sub_score_name] <-
        get_score(sub_keys, data_subitems)
    }
  }
  if (is_newvar) {
    data <- GLAD_rename(data, googlesheet,
      from = "easyname",
      to = "newvar"
    )
  }
  return(data)
}

GLAD_formula <- function(data, googlesheet, questionnaire) {
  is_newvar <- any(colnames(data) %in% googlesheet[["newvar"]])
  if (is_newvar) {
    # The formulae are in Easy.name, so if the data frame has New.variable
    # names it should be renamed first.
    data <- GLAD_rename(data, googlesheet,
      from = "newvar",
      to = "easyname"
    )
  }
  vars <- googlesheet[["easyname"]]

  derive_where <- grepl("Derived.variable", googlesheet[["Comments"]]) &
    googlesheet[["formula"]] != questionnaire &
    !googlesheet[["formula"]] %in% unique(googlesheet[["subscale"]])

  derive_vars <- vars[derive_where]

  for (dv in derive_vars) {
    formula <- sheet_extract("formula", dv, googlesheet) %>%
      parse(text = .)
    data[dv] <- with(data, eval(formula))
  }
  # If we've done the renaming, rename it back.
  if (is_newvar) {
    data <- GLAD_rename(data, googlesheet,
      from = "easyname",
      to = "newvar"
    )
  }
  return(data)
}

#' Generate Derived Variables For a Questionnaire Data Set.
#'
#' Generates derived variables with names and formulae specified in the
#' GLAD dictionary
#'
#' @param data A dataframe produced by 'GLAD_clean' containing variables of
#' a questionnaire.
#' @param googlesheet A googlesheet produced by "GLAD_sheet" that contain
#' the dictionary sheet of the variables in 'data'.
#' @return A dataframe with derived variable attached.
#' @export
GLAD_derive <- function(data, googlesheet) {
  # Get the name of the questionnaire
  questionnaire <- get_questionnaire(googlesheet)
  browser()
  data <- data %>%
    GLAD_score(googlesheet, questionnaire = questionnaire) %>%
    GLAD_formula(googlesheet, questionnaire = questionnaire)
  return(data)
}
