#' @name import_all
#' @title Import all tempo files from a folder.
#' @description
#' @param filetype The type of files in your directory. Currently need to be tab delimited.
#' Imports all tempo files from a certain folder.
#' Converts tempo data if needed.
#' @export

import_all <- function(filetype){
  temp <- list.files(pattern=filetype)
  myfiles <- lapply(temp, tempo_calc)

  tempo_calc <- function(x, delim="\t"){
    my_file <- read.delim(x, sep=delim, header=F)
    my_file$filename <- tools::file_path_sans_ext(basename(x))
    ms2bpm(my_file, ms=my_file$V1, cumulative=T, seconds = T)
  }
  tmp <- lapply(temp, tempo_calc)
  return(as.data.frame(do.call(rbind, tmp)))
}

# tmp <- lapply(temp, tempo_calc)
# library(tidyverse)
# df <- as.data.frame(do.call(rbind, tmp))
# df %>% separate(filename, c("year", "perfomer", "mvt"))
