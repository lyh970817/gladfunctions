#' Plot Variables in GLAD Data
#'
#' Generates plots for the specified variable in the GLAD data.
#'
#' @param data The dataframe containing the variable to be plotted.
#' @param var A character string of one element specifying the variable to
#' be plotted
#' @param title A character string. Only applies to categorical
#' variables that allow multiple options to be selected.
#' @param include_outlier A logical. Whether to include cases exceeding the
#' minimum and maximum in the dictionary, if they exist in 'data'.
#' @param binwidth A number or a character string "FD". Only applies to
#' continuous variables. If specified as "FD", the Freedman-Diaconis rule
#' for deciding optimal binwidth will be applied.
#' @param googlesheet A googlesheet produced by "GLAD_sheet" that contain
#' the dictionary sheet of the variables in 'data'.
#' @return For non-continuous variables, a single ggplot object. For continuous
#' variables, a named list of ggplot objects.
#' @export
GLAD_plot <- function(data, var, title = "", include_outlier = TRUE, binwidth = "", googlesheet) {
  if (!var %in% colnames(data)) {
    stop(var, " is not in the data frame.")
  }
  if (var == "Sex") {
    return(sex_plot(data))
  }
  type <- sheet_extract("type", var, googlesheet)
  catevars <- get_categvars(var, googlesheet)
  if (type == "Numeric/Continuous") {
    plots <- list()
    plots[["point"]] <- continous_point_plot(data, var, googlesheet, include_outlier)
    plots[["hist"]] <- hist_count(data, var, googlesheet, include_outlier, binwidth)
    plots[["density"]] <- density_plot_all(data, var, googlesheet, bysex = FALSE, include_outlier, binwidth)
    plots[["densitybysex"]] <- density_plot_all(data, var, googlesheet, bysex = TRUE, include_outlier, binwidth)
    return(plots)
  }
  if (type == "Binary" | type == "Categorical") {
    if (length(catevars) == 1) {
      return(factor_plot(data, var, googlesheet))
    }
    if (length(catevars) > 1) {
      return(categorical_plot(data, var, title = "", googlesheet))
    }
  }
}
