#' @name tempo_over_time
#' @title Plots average tempo and flexibility over time.
#' @description
#' This function just uses one of my most commonly
#' used ggplot graphs, plotting tempo over time.
#'
#' @param data, The dataframe
#' @param year, The year, or any other timeframe variable. Required.
#' @param average_tempo, The average tempo, as provided in the dataframe.
#' @param flex, flexibility, as determined by the standard deviation of the entire unit.

tempo_over_time <- function(data,
                            method = "lm",
                            big_title="Data over Time",
                            brief_subtitle="Average Tempo and Flexibility",
                            small_caption="Flexibility is measured as the standard
                            deviation of the entire unit."
                            ){
  summary_w_flex <- group_by(data, conductor) %>%
      summarise(
        average_tempo = mean(bpm, na.rm = T),
        flex = sd(bpm, na.rm = T),
        year = mean(year, na.rm = T))

  big_title <- big_title
  brief_subtitle <- brief_subtitle
  small_caption <- small_caption
  x_label <- "Year of Recording"
  y_label <- "Average Tempo"

  p <- ggplot2::ggplot(data = summary_w_flex,
      mapping = ggplot2::aes(x=year, y=average_tempo,label = performer)) +
      geom_point(ggplot2::aes(size = flex))

  p + ggplot2::geom_point() + ggrepel::geom_text_repel() +
  ggplot2::labs(x = x_label, y = y_label,
      title = big_title, subtitle = brief_subtitle,
      caption = small_caption) +
      ggplot2::stat_smooth(method = method, col = "blue") +
      ggplot2::scale_colour_grey(end = 2) + ggplot2::theme_bw()
}


