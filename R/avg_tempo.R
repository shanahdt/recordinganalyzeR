#' @name avg_tempo
#' @title Average Tempo
#' @description
#' This function provides an easy eay to get the average tempo by a number of groups.
#' You can look at tempo by measure, beat, composer, or piece. It returns a dataframe, not a tibble.
#'
#' @param df The dataframe
#' @param grouping grouping operator
#' @param selection Filtered selection.
#' @param tempo Whatever your tempo column is called.
#' @param year Whatever your year column is called.
#' @author Daniel Shanahan
#' @export

avg_tempo <- function(df, tempo, performer, year, add_flex = F){
  no_flex <- function(){
    df %>%
      group_by(performer) %>%
      summarise(tempo = mean(tempo), year=mean(year))
  }

  with_flex <- function(){
    df %>%
      group_by(performer) %>%
      summarise(flex=sd(tempo, na.rm=T), year=mean(year), tempo=mean(tempo))

  }
  if(add_flex==FALSE){
    return(as.data.frame(no_flex()))
  }
  else{
    return(as.data.frame(with_flex()))
  }
}


