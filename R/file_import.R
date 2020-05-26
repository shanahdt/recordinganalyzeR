library(recordinganalyzeR)
setwd("~/Box/CSV Tap Layers/")

walter <- read.csv("Early Recordings/Mvt_1/1939.walter.mvt1.csv", sep = "\t", header = F)
all_data <- read.csv("data_analysis/movement_1_data_a.csv", header=F)


##adding data
walter$year <- 1939
walter$performer <- "walter"
walter$mvt <- "mvt1"
walter$measure.beat <- walter$V2

#here's the new ms2bpm function from recordinanalyzer
walter <- ms2bpm(walter, ms = V1, cumulative = T) %>%
  mutate(bpm = bpm/1000)

#this is just me renaming columns to be consistent.
all_data$year <- all_data$V1
all_data$performer <- all_data$V2
all_data$tempo <- all_data$V4
all_data$measure.beat <- all_data$V5
all_data$bpm <- all_data$V8
all_data$diff <- all_data$V6

####these lines find the common columns and binds them all together.
common_cols <- intersect(colnames(all_data), colnames(walter))
rbind(
  all_data[common_cols],
  walter[common_cols]
)
