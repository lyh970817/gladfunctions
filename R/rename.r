test_nonames <- function(sheet, pos, from = "oldvar", to = "newvar") {
  # Test if there is a one-to-one mapping between Qualtrics.variable nad
  # New.variable, and between New.variable and Easy.name.

  # Need to completely rewrite this function. Use the function in my
  # iteration r bootcamp session

  oldvar <- sheet[[from]][pos]
  newvar <- sheet[[to]][pos]

  no_names <- c()
  for (i in seq_along(unique(oldvar))) {
    each_name <- unique(oldvar)[i]
    no_names[i] <- length(unique(newvar[oldvar == each_name]))
  }

  # no_names should of length 1, meaning each oldvar corresponds to one
  # newvar.

  if (length(unique(no_names)) != 1) {
    uniq_repname <- unique(oldvar)[which(no_names != 1)]

    for (name in uniq_repname) {
      corres_name <- unique(newvar[oldvar == name])
      message(paste(
        from, name, "has multiple corresponding",
        to, ":", paste(corres_name, collapse = ", ")
      ))
      if (from == "oldvar") {
        message(
          "Is this a variable with underscores that allows multiple options ?"
        )
      }
    }
  }
  no_names <- c()
  for (i in seq_along(unique(newvar))) {
    each_name <- unique(newvar)[i]
    no_names[i] <- length(unique(oldvar[newvar == each_name]))
  }

  # no_names should again be of length 1, meaning each newvar corresponds to one
  # oldvar.

  if (length(unique(no_names)) != 1) {
    uniq_repname <- unique(newvar)[which(no_names != 1)]

    for (name in uniq_repname) {
      corres_name <- unique(oldvar[newvar == name])
      message(paste(
        to, name, "has multiple corresponding",
        from, ":", paste(corres_name, collapse = ", ")
      ))
      if (to == "newvar") {
        message(
          "Is this a variable with underscores that allows multiple options ?"
        )
      }
    }
  }
}

GLAD_rename <- function(data, googlesheet, from = "oldvar", to = "newvar") {
  in_glad_oldvar <- googlesheet[[from]][!is.na(googlesheet[["GLAD.t0"]])]
  in_glad_newvar <- googlesheet[[to]][!is.na(googlesheet[["GLAD.t0"]])]

  oldvar <- googlesheet[[from]]
  newvar <- googlesheet[[to]]

  # People might leave some names blank so we use !is.na() and only
  # rename those oldvars that have newvars (and are also in the data
  # frame).

  name_pos <-
    which(oldvar %in% colnames(data) & !is.na(newvar))

  test_nonames(googlesheet, name_pos, from = from, to = to)

  oldnames <- googlesheet[[from]][name_pos] %>% unique()
  newnames <- googlesheet[[to]][name_pos] %>%
    unique() %>%
    setNames(oldnames)

  questionnaire <- get_questionnaire(googlesheet)

  nonames <-
    oldvar[is.na(newvar) & !grepl(
      "[Dd]erived",
      googlesheet[["Comments"]]
    ) & !is.na(googlesheet[["GLAD.t0"]])] %>%
    subset(!is.na(.))

  nonames <- nonames[nonames %in% in_glad_oldvar]

  if (length(nonames) > 0) {
    message(paste(
      questionnaire, "has", from, "names that do not have", to, "names:",
      paste(nonames, collapse = ", ")
    ))
  }
  data_renamed <- data %>% plyr::rename(newnames)
  return(data_renamed)
}
