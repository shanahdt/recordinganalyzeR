#' @name measure_select
#' @title Select Measures
#' @description
#' This is basically a wrapping of the dplyr filter program, but a little more straightforward (and therefore less flexible and powerful).
#'
#' @param df The dataframe
#' @param from where you want the selection to begin
#' @param to where you want the selection to end
#' @param measure.beat what your measure column is called.
#' @author Daniel Shanahan
#' @export

measure_select <- function(df, from, to, measure.beat="measure.beat"){
  dplyr::filter(df, measure.beat >= from & measure.beat <= to+1)

}

