add_numeric <- function(data_cleaned, data_raw) {
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
  return(data_cleaned)
}

to_csv <- function(data_cleaned, data_raw) {

  # Get the factor positions to have these changed to data in the raw file
  # (which is of numeric type) for csv export (`factor` will be exported
  # as character strings in csv)

  which_factor <- which(map_lgl(data_cleaned, is.factor))
  data_cleaned[which_factor] <- data_raw[which_factor]
  return(data_cleaned)
}

GLAD_export <- function(data_cleaned, data_raw, questionnaire, dirpath, googlesheet, rename = FALSE) {
  data_cleaned_csv <- to_csv(data_cleaned, data_raw)
  if (rename == TRUE) {
    questionnaire <- paste0(questionnaire, "_Renamed")

    data_easyname_csv <- GLAD_rename(data_cleaned_csv,
      googlesheet = googlesheet,
      from = "newvar",
      to = "easyname"
    )

    csv_name <- paste0("csv_renamed/", questionnaire, ".csv")
    csv_path <- paste0(dirpath, csv_name)
    write_csv(
      x = data_easyname_csv,
      path = csv_path
    )

    data_easyname_numeric <- GLAD_rename(data_cleaned,
      googlesheet = googlesheet,
      from = "newvar",
      to = "easyname"
    ) %>%
      add_numeric(data_raw)

    rds_name <- paste0("rds_renamed/", questionnaire, ".rds")
    rds_path <- paste0(dirpath, rds_name)
    saveRDS(
      data_easyname_numeric,
      file = rds_path
    )
  } else {
    csv_name <- paste0("csv/", questionnaire, ".csv")
    csv_path <- paste0(dirpath, csv_name)
    write_csv(
      x = data_cleaned_csv,
      path = csv_path
    )

    data_cleaned_numeric <- add_numeric(data_cleaned, data_raw)
    rds_name <- paste0("rds/", questionnaire, ".rds")
    rds_path <- paste0(dirpath, rds_name)
    saveRDS(
      data_cleaned_numeric,
      file = rds_path
    )
  }
}
