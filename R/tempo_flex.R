#' This function provides a flexibility rating (as measured with a standard
#' deviation of the entire unit given). It uses dplyr, and requires the year.
#'
#' @param tempo  The bpm data; it's own column.
#' @param flex The flexibility rating; a standard deviation of the tempi.
#' @param year The year column.
#'
#' @example
#' tempo_flex(movement1, conductors)
#'

tempo_flex <- function(data, performer="performer", tempo="tempo"){
  group_by(data, performer) %>%
  summarise(
    average_tempo = mean(tempo, na.rm = T),
    flex = sd(tempo, na.rm = T),
    year = mean(year, na.rm = T))
}
