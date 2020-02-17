GLAD_recode <- function(x, var, googlesheet, limits) {
  if (all(is.na(sheet_extract("newvar", var, googlesheet)))) {
    # `var` comes from the `imap_dfc` later so is a column name.
    message(paste(
      var, "is in the dataframe but not in New.variable."
    ))
    return(x)
  }

  type <- sheet_extract("type", var, googlesheet) %>% unique()

  if (length(type) > 1) {
    message(paste(
      var, "has more than one type :",
      paste(type, collapse = ", ")
    ))
    type <- "NA"
  }

  if (is.na(type)) {
    message(paste(var, "has no type information."))
    return(x)
  }

  if (type == "Categorical" | type == "Binary") {
    levels <- tryCatch(
      expr = {
        sheet_extract("levels", var, googlesheet) %>%
          as.numeric()
      },
      warning = function(w) {
        # `as.numeric()` converts numeric strings such as "13" to
        # double, but produces a warning and returns NA if it's a
        # character string
        message(var, " does not have numeric levels.")
        return("non-numeric")
      }
    )
    labels <- c(sheet_extract("labels", var, googlesheet))

    # If any level is not appropriate we leave the variable unchanged.

    if (any(!is.na(levels) & levels == "non-numeric")) {
      return(x)
    }
    if (length(levels) == 1) {
      message(paste(var, "has only one level."))
      return(x)
    }
    if (length(unique(levels)) != length(levels)) {
      message(paste(var, "does not have distinct levels."))
      return(x)
    }
    if (any(is.na(levels))) {
      message(paste(var, "has NA levels."))
      return(x)
    }
    if (any(is.na(labels))) {
      message(paste(var, "has NA labels"))
      return(x)
    }

    if (is.numeric(levels)) {
      level_increasing <-
        min(levels[levels >= 0]):max(levels[levels >= 0])
      level_decreasing <-
        max(levels[levels >= 0]):min(levels[levels >= 0])
      # levels >= 0 since levels can be -88 or -99.
      if (length(levels[levels >= 0]) != length(level_increasing)) {
        message(paste(var, "have levels with steps unequal to one."))
        # There are a few special cases in the data sets where this is not
        # an error (e.g. AUDIT).
      }
      if (any(levels[levels >= 0] != level_increasing) & any(levels[levels >= 0] != level_decreasing)) {
        message(paste(var, "does not have levels in consecutive order."))
        # There are a few special cases in the data sets where this is not
        # an error (e.g. AUDIT).
      }
    }

    # If a question is seen but not answered and it is not a categorical
    # question that allow multiple options to be selected, the response is automatically
    # exported as -99, but this might not be in the dictionary.
    if (!-99 %in% levels) {
      levels <- c(levels, -99)
      labels <- c(labels, "Prefer not to say")
    }

    if (any(!unique(x)[!is.na(unique(x))] %in% levels)) {
      message(paste(
        var, "has levels that are not in the dictionary.",
        "\nThe levels in dictionary are", paste(sort(levels), collapse = ", "),
        "\nand the levels in data are",
        paste(sort(unique(x)[!is.na(unique(x))]), collapse = ", ")
      ))
      return(x)
    }

    if (length(levels) != length(labels)) {
      message(var, " does not have levels and labels of the same length.")
      return(x)
    }

    x <- tryCatch(expr = {
      factor(x, levels = levels, labels = labels)
    }, error = function(e) {
      msg <- paste(
        "Error occurs at", var,
        "with levels:",
        paste(levels, collapse = ", "),
        "and labels:",
        paste(labels, collapse = ", ")
      )
      stop(msg)
    })
  } else if (type == "Numeric/Continuous") {
    x <- tryCatch(
      {
        as.numeric(x)
      },
      warning = function(w) {
        # The numeric items have text columns in Qualtrics so participants
        # can put it text which causes a warning when using
        # `as.numeric()`. We catch it so the user knows where the error
        # occurs.
        message("Non-numeric response in ", var)
        return(as.numeric(x))
      }
    )

    if (limits == FALSE) {
      return(x)
    }

    min_raw <- unique(sheet_extract("min", var, googlesheet))
    max_raw <- unique(sheet_extract("max", var, googlesheet))

    # if (is.na(min_raw)) {
    #   message(paste(var, "does not have min"))
    # }
    # if (is.na(max_raw)) {
    #   message(paste(var, "does not have max"))
    # }

    min <- as.numeric(min_raw)
    max <- as.numeric(max_raw)

    # If min_raw or max_raw is not a numeric string it should be a formula.
    # if (!grepl("[0-9]*", min_raw) & !is.na(min_raw)) {
    #   print(var)
    #   print(min_raw)
    #   min_formula <- parse(text = min_raw)
    #   min <- with(data, eval(min_formula))
    # }

    # if (!grepl("[0-9]*", max_raw) & !is.na(max_raw)) {
    #   print(var)
    #   print(max_raw)
    #   max_formula <- parse(text = max_raw)
    #   max <- with(data, eval(max_formula))
    # }

    unit <- sheet_extract("unit", var, googlesheet) %>% unique()
    # Errors with units do not cause issue with data cleaning, only matters for
    # plotting.
    if (length(unit) == 0) {
      message(paste(var, "is numeric but has no Unit column."))
    }
    else if (length(unit) > 1) {
      message(paste(var, "has more than one Unit."))
    }
    else if (is.na(unit)) {
      message(paste(var, "is numeric but has no Unit."))
    }

    # Some participants enter year as age, compute age for them
    # Need to make sure the these units actually correspond to age
    # variable.
    else if (grepl("[Aa]ge|[Yy]ears", unit)) {
      # message(paste(var, "is age"))
      # If age need logic we can identify the variables with this.
      # x[which(x > 1900 & x < year(Sys.Date()))] <-
      #   year(Sys.Date()) - x[which(x > 1900 & x < year(Sys.Date()))]
    }

    x[which(x < min | x > max)] <- NA
  }
  return(x)
}

GLAD_recode_df <- function(data, googlesheet, limits) {
  data_cleaned <- data %>%
    select(-ExternalReference, -Sex, -Age, -Birthyear) %>%
    GLAD_rename(googlesheet = googlesheet) %>%
    imap_dfc(GLAD_recode, googlesheet = googlesheet, limits) %>%
    bind_cols(data[c(
      "ExternalReference",
      "Sex",
      "Age",
      "Birthyear"
    )]) %>%
    # Reorder the column names.
    select(ID = ExternalReference, Sex, Age, Birthyear, everything())
  return(data_cleaned)
}
