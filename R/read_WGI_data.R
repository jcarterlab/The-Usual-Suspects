library(dplyr)
library(tidyverse)
library(jsonlite)
library(readxl)
library(stringr)

# sets the working directory. 
setwd("C:/Users/HUAWEI/Desktop/projects/The-Usual-Suspects")

# reads in the Worldwide Governance Indicators data. 
data <- read_xls("Data/WGI/WGI_indicators.xls",
                 range = "A14:ED229")

# filters the data so that only the scores and not other stats remain.  
new_data <- data
colnames(new_data) <- data[1,]
row_nums <- which(str_detect(colnames(new_data), "Estimate"))
data_frame <- data[,c(1, row_nums)]

# gets rid of the the first row which is now useless for our purposes. 
df <- data_frame[-1,]

# renames the columns for simplicity. 
new_names <- list()
for(i in 1:length(colnames(df))) {
  new_names[i] <- str_sub(colnames(df[i]), 1, 4)
}
colnames(df) <- unlist(new_names)

final_data <- df %>%
  rename("country" = 1) %>%
  gather(key=year, value=value, -1)

colnames(final_data) <- c("country", "year", "gov_effectivness")

# limits the years to a single decade between 2010 and 2019. 
final_data <- final_data %>%
  filter(year >= 2010 & year <=2019)

# replaces na entries with proper na values. 
for(i in 1:nrow(final_data)) {
  final_data[i,3] <- if(final_data[i,3]=="#N/A") {
    NA
  } else {
    final_data[i,3]
  }
}

# saves the data to a csv file. 
write_csv(final_data, "Data/combined data/WGI_data.csv")