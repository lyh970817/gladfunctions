# GLAD_qplot <- function(data, googlesheet) {
#     continuous_var <- googlesheet[["newvar"]][which(googlesheet[["type"]] == "Numeric/Continuous")]
#     for (i in seq_along(continuous_var)) {
#        print(qplot(sample = !!sym(continuous_var[i]), data))
#     }
# }

#' Generates Quantile Plot for a Continuous Variable
#'
#' Generates quantile plots for a specified variable in the GLAD data.
#'
#' @param data The dataframe containing the variable to be plotted.
#' @param var A character string of one element specifying the variable to
#' be plotted
#' @return A ggplot object.
#' @export
GLAD_qplot <- function(data, var) {
  return(ggplot(data, aes_string(sample = var)) + stat_qq())
}

# GLAD_vartest <- function(data, googlesheet) {
#   newvars <- googlesheet[["newvar"]]
#   easynames <- googlesheet[["easyname"]]
#   if (any(colnames(data) %in% newvars)) {
#     continuous_vars <- googlesheet[["newvar"]][which(googlesheet[["type"]] == "Numeric/Continuous")] %>%
#       unique() %>%
#       na.omit()
#   }
#   if (any(colnames(data) %in% easynames)) {
#     continuous_vars <- googlesheet[["easyname"]][which(googlesheet[["type"]] == "Numeric/Continuous")] %>%
#       unique() %>%
#       na.omit()
#   }
#   for (var in continuous_vars) {
#     formula <- formula(paste(var, "~ Sex"))

#     print(leveneTest(formula, data))
#     print(fligner.test(formula, data))
#     print(bartlett.test(formula, data))
#   }
# }

#' Runs Variance Tests Between Genders
#'
#' Runs several variance tests (Levene's test, Fligner's test and Barlett's
#' test) for comparing variance across gender.
#'
#' @param data The dataframe containing the variable to be plotted.
#' @param var A character string of one element specifying the variable to
#' be tested.
#' @export
GLAD_vartest <- function(data, var, googlesheet) {
  formula <- formula(paste(var, "~ Sex"))

  print(leveneTest(formula, data))
  print(fligner.test(formula, data))
  print(bartlett.test(formula, data))
}
