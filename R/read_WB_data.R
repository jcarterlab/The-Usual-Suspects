library(dplyr)
library(tidyverse)
library(jsonlite)
library(readxl)
library(stringr)

# sets the working directory. 
setwd("C:/Users/HUAWEI/Desktop/projects/The-Usual-Suspects")

# reads in the World Bank data. 
read_WB_data <- function(indicator_name) {
  data <- read_xls(paste0("Data/WB/", indicator_name, ".xls"),
                   range = "A4:BM271") %>%
    gather(key = "year", 
           value = indicator_name,
           5:65) %>%
    select(!c(2:4,))
  colnames(data) <- c("country",
                      "year",
                      indicator_name)
  return(data)
}

# creates a list of World Bank data sets. 
data_sets <- c("secondary_enrollment",
               "tertiary_enrollment",
               "infant_mortality",
               "life_expectancy",
               "gdp_per_capita",
               "gini_index",
               "poverty_1.90_USD",
               "high_tech_exports",
               "rd_researchers",
               "income_top_20_percent",
               "slums")

# reads in the data frames and stores them in a list. 
data <- list()
for(i in 1:length(data_sets)) {
  data[[i]] <- read_WB_data(data_sets[i])
}

# joins the data frames together by country and year. 
joined <- data %>% 
  reduce(left_join, by = c("country", "year"))

# limits the years to a single decade between 2010 and 2019. 
WB_data_frame <- joined %>%
  filter(year >= 2010 & year <=2019)

# saves the data to a csv file. 
write_csv(WB_data_frame, "Data/combined data/WB_data.csv")