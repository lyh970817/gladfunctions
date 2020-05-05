get_keys <- function(items, googlesheet) {
  map_dbl(items, function(x) {
    googlesheet[["score_key"]][which(googlesheet[["easyname"]] == x)][1] %>%
      as.numeric()
  })
}

get_score <- function(keys, data) {
  # We can freely `as.numeric` columns here as all the factor variables are
  # of `lfactor` class, which allows `as.numeric`
  data <- map_df(data, function(col) {
    ifelse(col == -99 | col == -77 | col == -88, NA, col) %>%
      return()
  })
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
  keys_pos <- which(googlesheet[["score_key"]] %in% c(1, -1))

  if (length(keys_pos) == 0) {
    message(paste(questionnaire, "has no Score.key."))
    return(data)
  }

  items <- vars[keys_pos] %>%
    unique() %>%
    .[. %in% colnames(data)]
  all_keys <- get_keys(items, googlesheet)
  browser()
  data_items <- data[paste(items, "numeric", sep = "_")]

  if (length(all_keys) >= 1) {
    if (!any(formulae == questionnaire, na.rm = T)) {
    } else {
      total_score_name <- vars[which(formulae == questionnaire)]

      data[total_score_name] <- tryCatch(
        get_score(all_keys, data_items),
        error = function(cond) {
          message("An error has occurred when scoring variable ", paste0("'", total_score_name, "'"), "in ", questionnaire)
          message(paste0(cond))
          return(NULL)
        }
      )
    }
  }

  subscales <- unique(googlesheet[["subscale"]]) %>%
    .[!is.na(.)] %>%
    .[. %in% googlesheet[["formula"]]]

  if (length(subscales > 1)) {
    for (subscale in subscales) {
      sub_items <-
        vars[which(googlesheet[["subscale"]] == subscale)] %>%
        unique() %>%
        .[. %in% colnames(data)]
      sub_keys <- get_keys(sub_items, googlesheet)
      data_subitems <-
        data_items[paste(sub_items, "numeric", sep = "_")]
      sub_score_name <- vars[which(googlesheet[["formula"]] == subscale)]

      data[sub_score_name] <- tryCatch(
        get_score(sub_keys, data_subitems),
        error = function(cond) {
          message("An error has occurred when scoring variable ", paste0("'", sub_score_name, "'"), "in ", questionnaire)
          message(paste0(cond))
          return(NULL)
        }
      )
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

  derive_where <- (grepl("Derived.variable", googlesheet[["Comments"]]) &
    googlesheet[["formula"]] != questionnaire &
    !googlesheet[["formula"]] %in% unique(googlesheet[["subscale"]])) %>%
    which()

  derive_vars <- vars[derive_where]
  for (dv in derive_vars) {
    formula <- tryCatch(
      sheet_extract("formula", dv, googlesheet) %>%
        parse(text = .),
      error = function(cond) {
        message("An error has occurred when deriving variable ", paste0("'", dv, "'"), "in ", questionnaire)
        message(paste0(cond))
        return(NULL)
      }
    )

    data[dv] <- tryCatch(
      with(data, eval(formula)),
      error = function(cond) {
        message("An error has occurred when deriving variable ", paste0("'", dv, "'"), "in ", questionnaire)
        message(paste0(cond))
        return(NULL)
      }
    )
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
  data <- data %>%
    GLAD_score(googlesheet, questionnaire = questionnaire) %>%
    GLAD_formula(googlesheet, questionnaire = questionnaire)
  return(data)
}
