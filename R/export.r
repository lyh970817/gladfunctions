get_path <- function(questionnaires, rename = FALSE, format, dirpath) {
  name <- paste0(format, "_renamed/", questionnaire, ".", format)
  path <- paste0(dirpath, name)
  return(path)
}

# To be put in GLAD_recode_df
to_binary <- function(data_cleaned, data_raw,
                      format, path) {
  # Add numeric version variables for factor varaibles.

  # These would have been recoded to factors in `data_cleaned` because in
  # the googlesheet they have labels of "Categorical" or "Binary". However,
  # they would have numeric values (instead of character strings which could
  # also have "Categorical" or "Binary" labels in googlesheet) in the raw
  # data file.

  which_numeric_factor <- which(map2_lgl(
    data_cleaned, data_raw,
    function(clean, raw) {
      is.factor(clean) & is.numeric(raw)
    }
  ))
  numeric_names <- paste0(names(which_numeric_factor), "_numeric")
  data_cleaned[numeric_names] <- data_raw[which_numeric_factor]

  if (format == "feather") {
    browser()
    write_feather(data_cleaned, path = path)
  }
  if (format == "rds") {
    saveRDS(data_cleaned, file = path)
  }
}

to_csv <- function(data_cleaned, data_raw, path) {

  # Get the factor positions to have these changed to data in the raw file
  # (which is of numeric type) for csv export (`factor` will be exported
  # as character strings in csv)

  which_factor <- which(map_lgl(data_cleaned, is.factor))
  data_cleaned[which_factor] <- data_raw[which_factor]
  write_csv(x = data_cleaned, path = path)
}

to_haven <- function(data_cleaned, format, path) {

  # Get the factor positions to have these changed to data in the raw file
  # (which is of numeric type) for csv export (`factor` will be exported
  # as character strings in csv)

  which_factor <- which(map_lgl(data_cleaned, is.factor))
  data_cleaned[which_factor] <- map(
    data_cleaned[which_factor],
    function(col) {
      col <- labelled(as.numeric(col),
        labels = llevels(col) %>% setNames(levels(col))
      )
      return(col)
    }
  ) %>%
    as.data.frame()
  # There would be a strange warning about `bind_rows` would cause haven
  # labelled information to lose here if `map_df` is usd instead

  if (format == "sav") {
    write_sav(data_cleaned, path)
  }
  if (format == "dta") {
    write_dta(data_cleaned, path)
  }
  if (format == "sas") {
    write_sas(data_cleaned, path)
  }
}


GLAD_export <- function(data_cleaned, data_raw, questionnaire, dirpath, googlesheet, format, rename) {
  if (length(format) > 1) {
    stop("Only one format allowed.")
  }
  if (!format %in% c("rds", "sav", "sas", "feather", "csv")) {
    stop("format must be one of c('rds', 'sav', 'sas', 'feather', 'csv')")
  }

  if (rename == TRUE) {
    dirpath <- paste0(dirpath, format, "_renamed")
    dir.create(dirpath, showWarnings = FALSE)
    questionnaire <- paste0(questionnaire, "_Renamed")
    data_cleaned <- GLAD_rename(data_cleaned,
      googlesheet = googlesheet,
      from = "newvar",
      to = "easyname"
    )
  } else {
    dirpath <- paste0(dirpath, format)
    dir.create(dirpath, showWarnings = FALSE)
  }

  name <- paste0(questionnaire, ".", format)
  path <- file.path(dirpath, name)

  if (format %in% c("rds", "feather")) {
    to_binary(data_cleaned, data_raw, format = format, path)
  }
  if (format %in% c("dta", "sas", "sav")) {
    to_haven(data_cleaned, format = format, path)
  }
  if (format == "csv") {
    to_csv(data_cleaned, data_raw, path)
  }
}
