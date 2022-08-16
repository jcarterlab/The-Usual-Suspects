library(dplyr)
library(tidyverse)
library(jsonlite)
library(readxl)
library(stringr)

# sets the working directory. 
setwd("C:/Users/HUAWEI/Desktop/projects/The-Usual-Suspects")

# reads in the data sets. 
read_data <- function(data_frame) {
  data <- read_csv(paste0("Data/combined data/", data_frame, ".csv"))
  return(data)
}

# lists the data frames to be read in.
data_sets <- c("WB_data", "TI_data", "WGI_data", "UN_data")

# reads in the data frames and stores them in a list. 
data <- list()
for(i in 1:length(data_sets)) {
  data[[i]] <- read_data(data_sets[i]) %>%
    filter(!is.na(country))
}

# creates a list of indicators to not include.
unwanted_indicators <- c("World",
                         "Arab World",
                         "Caribbean small states",
                         "Central Europe and the Baltics",
                         "East Asia & Pacific",
                         "East Asia & Pacific (excluding high income)",
                         "Euro area",
                         "Europe & Central Asia",
                         "Europe & Central Asia (excluding high income)",
                         "European Union",
                         "Fragile and conflict affected situations",
                         "Heavily indebted poor countries (HIPC)",
                         "Latin America & Caribbean",
                         "Latin America & Caribbean (excluding high income)",
                         "Least developed countries: UN classification",
                         "Middle East & North Africa",
                         "Middle East & North Africa (excluding high income)",
                         "North America",
                         "OECD members",
                         "Other small states",
                         "Pacific island small states",
                         "Small states",
                         "South Asia",
                         "Sub-Saharan Africa",
                         "Sub-Saharan Africa (excluding high income)",
                         "High income",
                         "Low & middle income",
                         "Low income",
                         "Lower middle income",
                         "Middle income",
                         "Upper middle income",
                         "Not classified",
                         "Africa Eastern and Southern",
                         "Africa Western and Central",
                         "Early-demographic dividend",
                         "IBRD only",
                         "IDA & IBRD total",
                         "IDA total",
                         "IDA blend",
                         "IDA only",
                         "Late-demographic dividend",
                         "Pre-demographic dividend",
                         "Post-demographic dividend",
                         "East Asia & Pacific (IDA & IBRD countries)",
                         "Europe & Central Asia (IDA & IBRD countries)",
                         "Latin America & the Caribbean (IDA & IBRD countries)",
                         "Middle East & North Africa (IDA & IBRD countries)",
                         "South Asia (IDA & IBRD)",
                         "Sub-Saharan Africa (IDA & IBRD countries)")

# filter the year to those between 2010 and 2019. 
for(i in 1:length(data)) {
  data[[i]] <- data[[i]] %>%
    filter(year>2009 & year<2020)
}

# drops unwanted indicators from the WB country column. 
data[[1]] <- data[[1]] %>%
  filter(!country %in% unwanted_indicators)


# creates a data frame of World Bank country names.
WB_countries <- unique(data[[1]]$country)

# creates a data frame of Transparency International country names.
TI_countries <- unique(data[[2]]$country)

# creates a data frame of Worldwide Governance Indicators country names.
WGI_countries <- unique(data[[3]]$country)

# creates a data frame of UN country names.
UN_countries <- unique(data[[4]]$country)


# creates a list of non-identical country names. 
get_non_identical <- function(list_1, list_2) {
  data <- list_2[which(list_2 %in% list_1 == FALSE)]
  return(data)
}

# checks which countries are not in WB data. 
get_non_identical(WB_countries, TI_countries)
get_non_identical(WB_countries, WGI_countries)
get_non_identical(WB_countries, UN_countries)

# creates lists of countries to be re-assigned or dropped. 
TI_reassignments <- tibble(original = c("Hong Kong",
                                        "United States of America",
                                        "Bahamas",
                                        "Korea, South",
                                        "Saint Vincent and the Grenadines",
                                        "Saint Lucia",
                                        "Czechia",
                                        "Slovakia",
                                        "Gambia",
                                        "Egypt",
                                        "Kyrgyzstan",
                                        "Russia",
                                        "Laos",
                                        "Iran",
                                        "Congo",
                                        "Guinea Bissau",
                                        "Democratic Republic of the Congo",
                                        "Korea, North",
                                        "Venezuela",
                                        "Yemen",
                                        "Syria"),
                           new = c("Hong Kong SAR, China",
                                   "United States",
                                   "Bahamas, The",
                                   "Korea, Rep.",
                                   "St. Vincent and the Grenadines",
                                   "St. Lucia",
                                   "Czech Republic",
                                   "Slovak Republic",
                                   "Gambia, The",
                                   "Egypt, Arab Rep.",
                                   "Kyrgyz Republic",
                                   "Russian Federation",
                                   "Lao PDR",
                                   "Iran, Islamic Rep.",
                                   "Congo, Rep.",
                                   "Guinea-Bissau",
                                   "Congo, Dem. Rep.",
                                   "Korea, Dem. People's Rep.",
                                   "Venezuela, RB",
                                   "Yemen, Rep.",
                                   "Syrian Arab Republic"))

WGI_reassignments <- tibble(original = c("C?te d'Ivoire",
                                         "Cape Verde",
                                         "Korea, Dem. Rep.",
                                         "Jersey, Channel Islands"),
                            new = c("Cote d'Ivoire",
                                    "Cabo Verde",
                                    "Korea, Rep.",
                                    "Channel Islands"))

UN_reassignments <- tibble(original = c("Congo (Brazzaville)",
                                        "Congo (Kinshasa)",
                                        "Czechia",
                                        "Egypt",
                                        "Gambia",
                                        "Hong Kong S.A.R. of China",
                                        "Iran",
                                        "Ivory Coast",
                                        "Kyrgyzstan",
                                        "Laos",
                                        "Russia",
                                        "Slovakia",
                                        "South Korea",
                                        "Syria",
                                        "Venezuela",
                                        "Yemen"),
                           new = c("Congo, Rep.",
                                   "Congo, Dem. Rep.",
                                   "Czech Republic",
                                   "Egypt, Arab Rep.",
                                   "Gambia, The",
                                   "Hong Kong SAR, China",
                                   "Iran, Islamic Rep.",
                                   "Cote d'Ivoire",
                                   "Kyrgyz Republic",
                                   "Lao PDR",
                                   "Russian Federation",
                                   "Slovak Republic",
                                   "Korea, Rep.",
                                   "Syrian Arab Republic",
                                   "Venezuela, RB",
                                   "Yemen, Rep."))

change <- TI_reassignments %>%
  rbind(TI_reassignments) %>%
  rbind(WGI_reassignments) %>%
  rbind(UN_reassignments) %>%
  distinct()

drop <- c("Taiwan", "Anguilla", "Netherlands Antilles (former)",
          "Cook Islands", "French Guiana", "Martinique",
          "Niue", "R?union", "S?o Tom? and Principe",
          "Taiwan, China", "North Cyprus",
          "Palestinian Territories", "Somaliland region", "Taiwan Province of China")


# reassigns the country names for all data frames. 
reassign_countries <- function(data) {
  countries <- data
  for(i in 1:length(countries)) {
    countries[i] <- if(countries[i] %in% change$original) {
      change$new[which(countries[i]==change$original)]
    }
    else {
      countries[i]
    }
  }
  return(countries)
}

# reassigns all countries in all data frames. 
for(i in 1:length(data)) {
  data[[i]]$country <- reassign_countries(data[[i]]$country)
}

# drops the countries from the drop data frame. 
for(i in 1:length(data)) {
  data[[i]] <- if(mean(data[[i]]$country %in% drop)>0) {
    data[[i]][-which(data[[i]]$country %in% drop),]
  } else {
    data[[i]]
  }
}

# gets the average for each individual column grouped by country. 
get_col_averages <- function(data_frame) {
  cols <- data_frame[,-c(1, 2)]
  names <- colnames(cols[,1:ncol(cols)])
  
  new_data <- list()
  for(i in 1:ncol(cols)) {
    new_data[[i]] <- data_frame %>%
      group_by(country) %>%
      summarize(average = sum(eval(parse(text=names[i]))[!is.na(eval(parse(text=names[i])))]) / sum(!is.na(eval(parse(text=names[i])))))   
  }
  new_data <- new_data %>% reduce(left_join, by = "country")
  colnames(new_data) <- c(colnames(data_frame[,1]), names)
  return(tibble(new_data))
}

# gets the average of each column for each data frame.  
get_all_col_averages <- function(data) {
  df <- list()
  for(i in 1:length(data)) {
    df[[i]] <- get_col_averages(data[[i]])
  }
  return(df)
}

# saves the column averages data. 
averages <- get_all_col_averages(data)

# checks which WB countries are not in data. 
TI <- get_non_identical(TI_countries, WB_countries)
WGI <- get_non_identical(WGI_countries, WB_countries)
UN <- get_non_identical(UN_countries, WB_countries)

missing <- unique(c(TI, WGI, UN))

# creates a data frame of zeros for missing countries. 
create_padded_values <- function(data_frame) {
  # sets the column names with a value of NA. 
  cols <- data_frame[1,-1]
  for(i in 1:ncol(cols)) {
    cols[1,i] <- NA
  }
  # creates a list of missing countries with col values of NA. 
  added_countries <- list()
  for(i in 1:length(missing)) {
    added_countries[[i]] <- if(!missing[i] %in% data_frame$country) {
      tibble(country = missing[i]) %>%
        cbind(cols) %>%
        tibble()
    }
  }
  # combines all countries into 1 data frame if there are any missing. 
  if(length(added_countries)>0) {
    df <- added_countries %>% 
      reduce(rbind)
  } else {
    return(tibble(country = "No missing countries here!"))
  }
  return(df)
}

# creates a list of padded values for missing countries. 
padded_values <- list()
for(i in 1:length(averages)) {
  padded_values[[i]] <- create_padded_values(averages[[i]])
}

# adds the averages with the padded values. 
padded_values_added <- list()
for(i in 1:length(padded_values)) {
  padded_values_added[[i]] <- if(ncol(padded_values[[i]])>1) {
    averages[[i]] %>%
      rbind(padded_values[[i]])
  } else {
    averages[[i]]
  } 
}

# reorders the data frame according to country names. 
for(i in 1:length(padded_values_added)) {
  padded_values_added[[i]] <- padded_values_added[[i]][order(padded_values_added[[i]]$country),]
}

# joins the data frames together. 
joined_data <- padded_values_added %>%
  reduce(left_join) %>%
  tibble()

# gets how many NAs a certain row has. 
get_nas <- function(row) {
  nas <- list()
  for(i in 1:ncol(row)) {
    nas[i] <- if(is.na(row[,i])) 1 else 0
  }
  return(sum(unlist(nas)))
}

# returns all the countries with more NAs than the limit set. 
na_limit <- 6

high_na_countries <- list()
for(i in 1:nrow(joined_data)) {
  if(get_nas(joined_data[i,])>na_limit) high_na_countries <- append(high_na_countries, joined_data[i,]$country)
}
high_na_countries <- unlist(high_na_countries)

# drops the high na countries from the data frame.
na_dropped_data <- joined_data[-which(joined_data$country %in% high_na_countries),]

# saves the data to a csv file. 
write_csv(na_dropped_data, "Data/combined data/combined_data.csv")