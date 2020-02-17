library(tidyverse)
# Exclude -99 and -88
# -99 and -88 in subtitles

continuous_point_plot_wo_outliers <- function(data, var, unit, title, googlesheet) {
  min <- tryCatch(
    expr = {
      sheet_extract("min", var, googlesheet)
    },
    error = function(e) {
      min(data[[var]], na.rm = T)
    }
  )

  max <- tryCatch(
    expr = {
      sheet_extract("max", var, googlesheet)
    },
    error = function(e) {
      max(data[[var]], na.rm = T)
    }
  )

  if (length(min) > 1) {
    message(paste("More than one minimum for", var))
    min <- min[1]
  }

  if (length(max) > 1) {
    message(paste("More than one maximum for", var))
    max <- max[1]
  }

  if (!is.numeric(min)) {
    message(paste("Minimum for", var, "is not numeric."))
    min <- min(data[[var]], na.rm = T)
  }

  if (!is.numeric(max)) {
    message(paste("Maximum for", var, "is not numeric."))
    max <- max(data[[var]], na.rm = T)
  }

  length_missing <- sum(data[[var]] > max | data[[var]] < min, na.rm = T)
  data <- data %>% filter((data[[var]] <= max & data[[var]] >= min) | is.na(data[[var]]))

  continuous_point_plot_wo_outliers <- ggplot(
    data = data,
    aes(x = seq_along(data[[var]]), y = data[[var]])
  ) +
    geom_point(colour = "azure3") +
    ylim(0, max(data[[var]], na.rm = T) + 5) +
    labs(
      x = "Index",
      y = unit,
      title = title,
      subtitle = paste0(
        "Dashed line represents mean and blue dotted lines represent ± 1 standard deviation\n",
        "Red dotted lines represent ± 3.5 standard deviations\n",
        "n = ", sum(!is.na(data[[var]])),
        "; NA = ", sum(is.na(data[[var]])),
        "; n(total) = ", length(data[[var]]),
        "; n(outlier) removed =", length_missing
      ),
      color = "black"
    ) +
    theme(
      panel.grid.major.y = element_line(
        size = 0.5,
        linetype = "dashed",
        colour = "gray"
      ),
      axis.text.x = element_text(colour = "black", size = 12),
      axis.text.y = element_text(colour = "black", size = 12),
      axis.ticks.x = element_blank(),
      axis.ticks.y = element_blank(),
      panel.background = element_blank()
    ) +

    geom_hline(
      data = data, aes(yintercept = mean(data[[var]], na.rm = T)),
      size = 1, linetype = "dashed", colour = "deepskyblue4"
    ) +
    geom_hline(
      data = data, aes(yintercept = mean(data[[var]], na.rm = T) + sd(data[[var]], na.rm = T)),
      size = 1, linetype = "dotted", colour = "deepskyblue3"
    ) +
    geom_hline(
      data = data, aes(yintercept = mean(data[[var]], na.rm = T) - sd(data[[var]], na.rm = T)),
      size = 1, linetype = "dotted", colour = "deepskyblue3"
    ) +
    geom_hline(
      data = data, aes(yintercept = mean(data[[var]], na.rm = T) + 3.5 * sd(data[[var]], na.rm = T)),
      size = 1, linetype = "dotted", colour = "firebrick"
    ) +
    geom_hline(
      data = data, aes(yintercept = mean(data[[var]], na.rm = T) - 3.5 * sd(data[[var]], na.rm = T)),
      size = 1, linetype = "dotted", colour = "firebrick"
    )
  return(continuous_point_plot_wo_outliers)
}

continuous_point_plot_w_outliers <- function(data, var, unit, title) {
  continuous_point_plot <- ggplot(
    data = data,
    aes_string(x = seq_along(data[[var]]), y = var)
  ) +
    geom_point(colour = "azure3") +
    ylim(0, max(data[[var]], na.rm = T) + 5) +
    labs(
      x = "Index",
      y = unit,
      title = title,
      subtitle = paste0(
        "Dashed line represents mean and dotted lines represent  1 standard deviation\n",
        "n = ", sum(!is.na(data[[var]])),
        "; NA = ", sum(is.na(data[[var]])),
        "; n(total) = ", length(data[[var]])
      ),
      color = "black"
    ) +
    theme(
      panel.grid.major.y = element_line(
        size = 0.5,
        linetype = "dashed",
        colour = "gray"
      ),
      axis.text.x = element_text(colour = "black", size = 12),
      axis.text.y = element_text(colour = "black", size = 12),
      axis.ticks.x = element_blank(),
      axis.ticks.y = element_blank(),
      panel.background = element_blank()
    ) +

    geom_hline(
      data = data, aes(yintercept = mean(data[[var]], na.rm = T)),
      size = 1, linetype = "dashed", colour = "deepskyblue4"
    ) +
    geom_hline(
      data = data, aes(yintercept = mean(data[[var]], na.rm = T) + sd(data[[var]], na.rm = T)),
      size = 1, linetype = "dotted", colour = "deepskyblue3"
    ) +
    geom_hline(
      data = data, aes(yintercept = mean(data[[var]], na.rm = T) - sd(data[[var]], na.rm = T)),
      size = 1, linetype = "dotted", colour = "deepskyblue3"
    ) +
    geom_hline(
      data = data, aes(yintercept = mean(data[[var]], na.rm = T) + 3.5 * sd(data[[var]], na.rm = T)),
      size = 1, linetype = "dotted", colour = "firebrick"
    ) +
    geom_hline(
      data = data, aes(yintercept = mean(data[[var]], na.rm = T) - 3.5 * sd(data[[var]], na.rm = T)),
      size = 1, linetype = "dotted", colour = "firebrick"
    )
  return(continuous_point_plot)
}

continous_point_plot <- function(data, var, googlesheet, include_outlier = TRUE) {
  if (!is.data.frame(data)) stop("'data' needs to be an object of class 'data.frame'.")
  if (!is.character(var) & length(var) == 1) stop("'var' needs to be a character vector of length 1")

  title <- sheet_extract("title", var, googlesheet)

  if (length(title) > 1) {
    message("More than one title matching the variable name, only the first will be used.")
    title <- title[1]
  }

  unit <- sheet_extract("unit", var, googlesheet)

  if (length(unit) > 1) {
    message("More than one unit matching the variable name, only the first will be used.")
    unit <- unit[1]
  }

  if (include_outlier == TRUE) {
    return(continuous_point_plot_w_outliers(data, var, unit, title))
  } else if (include_outlier == FALSE) {
    return(continuous_point_plot_wo_outliers(
      data, var, unit, title,
      googlesheet
    ))
  }
}
