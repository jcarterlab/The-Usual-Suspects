library(dplyr)
library(tidyverse)
library(jsonlite)
library(readxl)
library(stringr)

# sets the working directory. 
setwd("C:/Users/HUAWEI/Desktop/projects/The-Usual-Suspects")

# reads in the UN data. 
data <- read_xls("Data/UN/happiness_data.xls",
                 range = "A1:L2090")

# creates a list of UN data sets. 
data_sets <- c("Positive affect",
               "Generosity",
               "Freedom to make life choices")

# selects the necessary columns. 
data_frame <- data[,c("Country name", "year", data_sets)] %>%
  rename(country = 1)

# renames the columns. 
colnames(data_frame) <- c("country", "year", "positive_affect", 
                          "generosity", "freedom")

# saves the data to a csv file. 
write_csv(data_frame, "Data/combined data/UN_data.csv")
