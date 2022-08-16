library(dplyr)
library(tidyverse)
library(jsonlite)
library(readxl)
library(stringr)

# sets the working directory. 
setwd("C:/Users/HUAWEI/Desktop/projects/The-Usual-Suspects")

# reads in the Transparency International data. 
data <- read_xls("Data/TI/corruption.xls",
                 range = "A3:AH183") %>%
  select(c(Country, contains("CPI score")))

# changes the column names. 
colnames(data) <- c("country", rev(seq(2012, 2020, 1)))

# drops the 2020 column. 
TI_data_frame <- data[,-which(colnames(data)=="2020")] %>%
  gather(key=year, value=value, -1)

# renames the columns 
colnames(TI_data_frame) <- c("country", "year", "corruption")

# saves the data to a csv file. 
write_csv(TI_data_frame, "Data/combined data/TI_data.csv")

