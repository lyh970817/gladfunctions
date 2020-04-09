GLAD_missing_freq <- function(data) {
  missing_per <- tb(freq(data["Percentage of NA Per Person"]))[1]
  colnames <- dimnames(freq(data["NA Per Person"]))[[2]]
  missing_freq <- tb(freq(data["NA Per Person"]))
  colnames(missing_freq)[-1] <- colnames
  missing_freq <- missing_freq %>%
    bind_cols(missing_per) %>%
    select(
      "NA Per Person",
      "Percentage of NA Per Person",
      everything()
    )

  kable(missing_freq, digits = 2) %>%
    kable_styling() %>%
    scroll_box(width = "100%", height = "300px")
}

GLAD_missing_plot <- function(data) {
  missing_plot <- ggplot(
    data = data,
    aes_string(x = "Items", y = "`Number of Items Missing`")
  ) +
    geom_point() +
    labs(
      y = "Missingness per item"
    ) +
    theme_minimal() +
    coord_flip()
  return(missing_plot)
}

GLAD_missing_kable <- function(data) {
  kable(
    table(
      data[["Items"]],
      data[["Percentage of Items Missing"]]
    ),
    digits = 2
  ) %>%
    kable_styling() %>%
    scroll_box(width = "100%", height = "300px")
}

GLAD_missing_descr <- function(data) {
  kable(descr(data["Percentage of Items Missing"],
    style = "rmarkdown"
  ),
  digits = 2
  ) %>%
    kable_styling() %>%
    scroll_box(width = "100%", height = "300px")
}

#' Examine Missingness of a Questionnaire
#'
#' Produces various summary statistics and plots for missingness
#' examination of a questionnaire.
#'
#' @param data A dataframe produced by 'GLAD_clean' containing variables of
#' a questionnaire.
#' @return A named list of summary statistics and plots.
#' @export
GLAD_missing <- function(data) {
  items <- names(data %>%
    select(-ID, -Sex, -contains("numeric")))

  missing <- as.data.frame(items, name = NULL)
  colnames(missing) <- "Items"
  missing["Number of Items Missing"] <- colSums(is.na(data[items]))
  missing["Percentage of Items Missing"] <- round(missing[["Number of Items Missing"]] / nrow(data), digits = 2)

  data["NA Per Person"] <- rowSums(is.na(data[items]))
  data["Percentage of NA Per Person"] <- round(x = data["NA Per Person"] / length(items), digits = 2)

  missing_list <- list()
  missing_list[["percent"]] <- mean(complete.cases(data[items]))
  missing_list[["descr"]] <- GLAD_missing_descr(missing)
  missing_list[["freq"]] <- GLAD_missing_freq(data)
  missing_list[["table"]] <- GLAD_missing_kable(missing)
  missing_list[["plot"]] <- GLAD_missing_plot(missing)

  return(missing_list)
}
