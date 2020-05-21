#' Read In Raw Data
#'
#' Read in all the Qualtrics exported raw csv files in the specified path.
#'
#' @param path A character string specifying a folder containing the raw
#' files.
#' @return A named list of dataframes each corresponding to a raw data file with
#' participant personal information removed.
#' @export
GLAD_read <- function(path) {
  raw_files <- Sys.glob(paste0(path, "*"))
  dat_list <- list()
  dat_names <- c()
  for (i in seq_along(raw_files)) {
    col_names <- read_csv(raw_files[[i]], n_max = 0) %>% names()
    # Need to skip 3 rows and add the column names later due to the redundant
    # rows in the file
    # Maybe don't read in the file twice... Change it later.
    dat_list[[i]] <- read_csv(raw_files[[i]],
      skip = 3, col_names = col_names, guess_max = 30000
    ) %>%
      select(-c(seq(3, 12), seq(14, 17)))

    dat_names[i] <-
      # Use the second name that mathces the pattern since the first match
      # could be IP address or something irrelevant.
      str_extract(colnames(dat_list[[i]]), "[A-Z]{2,}") %>%
      unique() %>%
      subset(!is.na(.)) %>%
      # Could match other things but the first is always correct.
      .[1]
  }

  names(dat_list) <- dat_names
  return(dat_list)
}
