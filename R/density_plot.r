palette <- c("#efc00b", "#b7dee8")

hist_count <- function(data, var, googlesheet, include_outlier = TRUE, binwidth) {
  title <- sheet_extract("title", var, googlesheet)

  if (length(title) > 1) {
    warning("More than one question matching the variable name, only the first will be used.")
    title <- title[1]
  }

  if (include_outlier == FALSE) {
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

    length_missing <- sum(data[[var]] > max | data[[var]] < min, na.rm = T)
    data <- data %>% filter((data[[var]] <= max & data[[var]] >= min) | is.na(data[[var]]))
  }

  unit <- sheet_extract("unit", var, googlesheet)

  var_sex <- data %>%
    group_by(Sex) %>%
    summarise(.,
      grp.mean = mean(!!sym(var), na.rm = TRUE),
      grp.sd = sd(!!sym(var), na.rm = TRUE),
      grp.n = sum(!is.na(!!sym(var)))
    ) %>%
    mutate(
      grp.labels.gen = paste0(prettyNum(round(grp.mean, 2), big.mark = ",")),
      labels.possd.gen = paste0(prettyNum(round(grp.mean + grp.sd, 2), big.mark = ",")),
      labels.negsd.gen = paste0(prettyNum(round(grp.mean - grp.sd, 2), big.mark = ","))
    )

  if (binwidth == "FD") {
    binwidth <- 2 * IQR(data[[var]], na.rm = TRUE) / length(na.omit(data[[var]]))^(1 / 3)
  } else {
    binwidth <- 1
  }

  hist_count_sex_base <- ggplot(
    data = data,
    aes_string(x = var)
  ) +
    geom_histogram(aes(color = Sex, fill = Sex, alpha = 0.5),
      binwidth = binwidth,
      position = "identity"
    ) +
    scale_fill_manual(values = palette) +
    scale_color_manual(values = palette) +
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
      panel.background = element_blank(),
      legend.position = "bottom"
    ) +
    guides(alpha = FALSE, color = FALSE) +
    geom_vline(data = var_sex, aes(xintercept = grp.mean, colour = Sex), size = 0.5, linetype = "dashed") +
    geom_vline(data = var_sex, aes(xintercept = grp.mean + grp.sd, colour = Sex), size = 0.5, linetype = "dotted") +
    geom_vline(data = var_sex, aes(xintercept = grp.mean - grp.sd, colour = Sex), size = 0.5, linetype = "dotted")

  if (include_outlier == TRUE) {
    hist_count_sex <- hist_count_sex_base +
      labs(
        x = unit,
        y = "Count",
        title = title,
        subtitle = paste0(
          "Dashed line represents mean and dotted lines represent  1 standard deviation\n",
          "n(female) = ", var_sex$grp.n[var_sex["Sex"] == "Female"],
          "; n(male) = ", var_sex$grp.n[var_sex["Sex"] == "Male"]
        ),
        fill = "Sex"
      )
  } else {
    hist_count_sex <- hist_count_sex_base +
      labs(
        x = unit,
        y = "Count",
        title = title,
        subtitle = paste0(
          "Dashed line represents mean and dotted lines represent  1 standard deviation\n",
          "n(female) = ", var_sex$grp.n[var_sex["Sex"] == "Female"],
          "; n(male) = ", var_sex$grp.n[var_sex["Sex"] == "Male"],
          "; n(outlier) removed =", length_missing
        ),
        fill = "Sex"
      )
  }
  return(hist_count_sex)
}

density_plot_bysex <- function(data, var, title, unit, googlesheet, include_outlier = TRUE, binwidth) {
  if (include_outlier == FALSE) {
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
  }

  density_data <- density(data[[var]], na.rm = T)

  var_sex <- data %>%
    group_by(Sex) %>%
    summarise(.,
      grp.mean = mean(!!sym(var), na.rm = TRUE),
      grp.sd = sd(!!sym(var), na.rm = TRUE),
      grp.n = sum(!is.na(!!sym(var)))
    ) %>%
    mutate(
      grp.labels.gen = paste0(prettyNum(round(grp.mean, 2), big.mark = ",")),
      labels.possd.gen = paste0(prettyNum(round(grp.mean + grp.sd, 2), big.mark = ",")),
      labels.negsd.gen = paste0(prettyNum(round(grp.mean - grp.sd, 2), big.mark = ","))
    )

  density_plot_bysex_base <- ggplot(
    data = data,
    aes_string(x = var)
  ) +
    geom_histogram(aes(y = ..density.., color = Sex, fill = Sex, alpha = 0.5),
      binwidth = binwidth
    ) +
    scale_fill_manual(values = palette) +
    scale_color_manual(values = palette) +
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
    guides(alpha = FALSE, color = FALSE, fill = FALSE)

  density_plot_bysex_base <- density_plot_bysex_base %>%
    ggformula::gf_fitdistr(dist = "dnorm", color = "darkgrey")

  density_plot_bysex_base <- density_plot_bysex_base +
    geom_vline(
      data = var_sex, aes(xintercept = grp.mean, colour = Sex),
      size = 0.5, linetype = "dashed"
    ) +
    geom_vline(
      data = var_sex, aes(xintercept = grp.mean + grp.sd, colour = Sex),
      size = 0.5, linetype = "dotted"
    ) +
    geom_vline(
      data = var_sex, aes(xintercept = grp.mean - grp.sd, colour = Sex),
      size = 0.5, linetype = "dotted"
    ) +
    geom_text_repel(
      data = var_sex, aes(x = grp.mean, y = max(density_data$y), label = grp.labels.gen),
      size = 3, hjust = -.1
    ) +
    geom_text_repel(
      data = var_sex,
      aes(
        x = grp.mean + grp.sd,
        y = max(density_data$y),
        label = labels.possd.gen
      ),
      size = 3, hjust = -.1
    ) +
    geom_text_repel(
      data = var_sex,
      aes(
        x = grp.mean - grp.sd,
        y = max(density_data$y),
        label = labels.negsd.gen
      ),
      size = 3, hjust = -.1
    ) +
    facet_wrap(. ~ Sex, nrow = 2)

  if (include_outlier == TRUE) {
    density_plot_bysex <- density_plot_bysex_base +
      labs(
        x = unit,
        y = "Count",
        title = title,
        subtitle = paste0(
          "Dashed line represents mean and dotted lines represent  1 standard deviation\n",
          "n(female) = ", var_sex$grp.n[var_sex["Sex"] == "Female"],
          "; n(male) = ", var_sex$grp.n[var_sex["Sex"] == "Male"]
        ),
        fill = "Sex"
      )
  } else {
    density_plot_bysex <- density_plot_bysex_base +
      labs(
        x = unit,
        y = "Count",
        title = title,
        subtitle = paste0(
          "Dashed line represents mean and dotted lines represent  1 standard deviation\n",
          "n(female) = ", var_sex$grp.n[var_sex["Sex"] == "Female"],
          "; n(male) = ", var_sex$grp.n[var_sex["Sex"] == "Male"],
          "; n(outlier) removed =", length_missing
        ),
        fill = "Sex"
      )
  }
  return(density_plot_bysex)
}

density_plot <- function(data, var, title, unit, googlesheet, include_outlier = FALSE, binwidth) {
  if (include_outlier == FALSE) {
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

    length_missing <- sum(data[[var]] > max | data[[var]] < min, na.rm = T)
    data <- data %>% filter((data[[var]] <= max & data[[var]] >= min) | is.na(data[[var]]))
  }

  # Create a column for nice looking labels to add to the graphics
  mean.combined <- data %>%
    summarise(
      mean.combined = mean(data[[var]], na.rm = T),
      sd.combined = sd(data[[var]], na.rm = T),
      sd.3.5.combined = 3.5 * sd(data[[var]], na.rm = T)
    ) %>%
    mutate(
      labels.mean.gen = paste0(prettyNum(round(mean.combined, 2), big.mark = ",")),
      labels.possd.gen = paste0(prettyNum(round(mean.combined + sd.combined, 2), big.mark = ",")),
      labels.negsd.gen = paste0(prettyNum(round(mean.combined - sd.combined, 2), big.mark = ",")),
      labels.possd.3.5.gen = paste0(prettyNum(round(mean.combined + sd.3.5.combined, 2), big.mark = ",")),
      labels.negsd.3.5.gen = paste0(prettyNum(round(mean.combined - sd.3.5.combined, 2), big.mark = ","))
    )

  # Calculate density values for the mean labels
  density_data <- density(data[[var]], na.rm = T)

  density_plot_base <- ggplot(
    data = data,
    aes_string(x = var)
  ) +
    geom_histogram(aes(y = ..density..),
      fill = "white", colour = "black",
      binwidth = binwidth
    ) +
    stat_function(
      fun = dnorm,
      color = "darkred",
      args = list(
        mean = mean(data[[var]], na.rm = TRUE),
        sd = sd(data[[var]], na.rm = TRUE)
      )
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
    geom_vline(data = data, aes(xintercept = mean(data[[var]], na.rm = T)), size = 0.5, linetype = "dashed") +
    geom_vline(data = data, aes(xintercept = mean(data[[var]], na.rm = T) + sd(data[[var]], na.rm = T)), size = 0.5, linetype = "dotted") +
    geom_vline(data = data, aes(xintercept = mean(data[[var]], na.rm = T) - sd(data[[var]], na.rm = T)), size = 0.5, linetype = "dotted") +
    geom_text_repel(
      data = mean.combined, aes(x = mean.combined, y = max(density_data$y), label = labels.mean.gen),
      size = 3, hjust = -.1
    ) +
    geom_text_repel(
      data = mean.combined, aes(x = mean.combined + sd.combined, y = max(density_data$y), label = labels.possd.gen),
      size = 3, hjust = -.1
    ) +
    geom_text_repel(
      data = mean.combined, aes(x = mean.combined - sd.combined, y = max(density_data$y), label = labels.negsd.gen),
      size = 3, hjust = -.1
    ) +
    geom_vline(data = data, aes(xintercept = mean(data[[var]], na.rm = T) + 3.5 * sd(data[[var]], na.rm = T)), size = 0.5, linetype = "dotted") +
    geom_vline(data = data, aes(xintercept = mean(data[[var]], na.rm = T) - 3.5 * sd(data[[var]], na.rm = T)), size = 0.5, linetype = "dotted") +
    geom_text_repel(
      data = mean.combined, aes(x = mean.combined + sd.3.5.combined, y = max(density_data$y), label = labels.possd.3.5.gen),
      size = 3, hjust = -.1
    ) +
    geom_text_repel(
      data = mean.combined, aes(x = mean.combined - sd.3.5.combined, y = max(density_data$y), label = labels.negsd.3.5.gen),
      size = 3, hjust = -.1
    )

  if (include_outlier == TRUE) {
    density_plot <- density_plot_base +
      labs(
        y = "Density",
        x = unit,
        title = title,
        subtitle = paste0(
          "Dashed line represents mean and dotted lines represent  1 standard deviation\n",
          "Outer dotted lines represent  3.5 standard deviations\n",
          "n = ", sum(!is.na(data[[var]])),
          "; NA = ", sum(is.na(data[[var]])),
          "; n(total) = ", length(data[[var]])
        ),
        color = "black"
      )
  } else {
    density_plot <- density_plot_base +
      labs(
        y = "Density",
        x = unit,
        title = title,
        subtitle = paste0(
          "Dashed line represents mean and dotted lines represent  1 standard deviation\n",
          "Outer dotted lines represent  3.5 standard deviations\n",
          "n = ", sum(!is.na(data[[var]])),
          "; NA = ", sum(is.na(data[[var]])),
          "; n(total) = ", length(data[[var]]),
          "; n(outlier) removed =", length_missing
        ),
        color = "black"
      )
  }
  return(density_plot)
}


density_plot_all <- function(data, var, googlesheet, bysex = FALSE, include_outlier = TRUE, binwidth = "") {
  title <- sheet_extract("title", var, googlesheet)
  unit <- sheet_extract("unit", var, googlesheet)
  if (binwidth == "FD") {
    binwidth <- 2 * IQR(data[[var]], na.rm = TRUE) / length(na.omit(data[[var]]))^(1 / 3)
  } else {
    binwidth <- 1
  }
  if (bysex == FALSE) {
    if (include_outlier == TRUE) {
      return(density_plot(data, var, title, unit, googlesheet, include_outlier = TRUE, binwidth))
    } else {
      return(density_plot(data, var, title, unit, googlesheet,
        include_outlier = FALSE, binwidth
      ))
    }
  } else {
    data <- subset(data, !is.na(data[["Sex"]]))
    if (include_outlier == TRUE) {
      return(density_plot_bysex(data, var, title, unit, googlesheet, include_outlier = TRUE, binwidth))
    } else {
      return(density_plot_bysex(data, var, title, unit, googlesheet,
        include_outlier = FALSE, binwidth
      ))
    }
  }
}
