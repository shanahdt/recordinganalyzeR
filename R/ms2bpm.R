#' @name ms2bpm
#' @title Milliseconds to Beats per Minute.
#' @description
#' This function converts the millisecond data into beats per minute data.
#'
#' @param data dataframe being examined
#' @param ms millisecond column name. This can be set up to either be consistent onsets between beats,
#' or a cumulative running total of milliseconds.  Which one it is should determine how you use the cumulative argument.
#' @param cumulative boolean that goes between a cumulative onset count,
#' as Sonic Visualizer will output, and one that is already
#' using the time between onsets in a non-cumulative way.
#' @export
#' @author Daniel Shanahan
#' @date May 2020

ms2bpm <- function(df, ms, cumulative=F){

      non_cumulative <- function(){
        ms <- enquo(ms)
        df %>%
        mutate(bpm = 60000/!!ms)
      }

      accruing <- function(){
        ms <- enquo(ms)
        df %>%
          mutate(diff = ifelse(!!ms == lag(!!ms), !!ms, !!ms - lag(!!ms)))
      } %>%
        mutate(bpm = 60000/diff)

      if(cumulative==FALSE){
      return(non_cumulative())
      }
      else{
        return(accruing())
      }
  }



