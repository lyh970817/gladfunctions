categorical_plot <- function(data, var, title = "", googlesheet) {
  cols <- get_categvars(var, googlesheet)

  labels <- map_chr(cols, function(col) {
    levels <- sheet_extract("levels", col, googlesheet)
    label <- sheet_extract("labels", col, googlesheet)[which(levels == 1)]
  })

  data[cols] <- map2_df(data[cols], labels, function(col, label) {
    col <- as.character(col)
    col[col == label] <- 1
    return(col)
  })

  names(cols) <- labels
  dat_gathered <- data %>%
    select(ID, Sex, cols) %>%
    gather(cols, value, -ID, -Sex)

  dat_plot <- dat_gathered %>%
    group_by(cols, Sex) %>%
    filter(value == 1) %>%
    count()

  n_total <- nrow(data)
  n <- sum(!data[cols] %>% is_rowna())
  n_missing <- n_total - n
  n_male <- sum(!data[cols] %>% is_rowna() & data[["Sex"]] == "Male")
  n_female <- sum(!data[cols] %>% is_rowna() & data[["Sex"]] == "Female")

  plot <- ggplot(data = dat_plot, mapping = (aes(x = cols, y = n, fill = Sex))) +
    geom_col(position = position_dodge()) +
    scale_fill_manual(values = palette) +
    aes(stringr::str_wrap(cols, 25)) +
    labs(
      y = "Frequency",
      title = title,
      subtitle = paste("n = ", n,
        "; n(total) = ", n_total,
        "; n(missing) = ", n_missing,
        "; n(female) = ", n_male,
        "; n(male) = ", n_female,
        sep = ""
      ),
      fill = "Sex"
    ) +
    theme(
      panel.grid.major.x = element_line(
        size = 0.5,
        linetype = "dashed",
        colour = "gray"
      ),
      axis.title.y = element_blank(),
      axis.text.x = element_text(colour = "black", size = 12),
      axis.text.y = element_text(colour = "black", size = 12),
      axis.ticks.x = element_blank(),
      axis.ticks.y = element_blank(),
      panel.background = element_blank(),
      legend.position = "bottom"
    ) +
    geom_text_repel(aes(label = dat_plot$n),
      position = position_dodge(width = 0.9), hjust = -0.25,
      color = "black", size = 4
    ) +
    coord_flip(clip = "on")

  return(plot)
}
