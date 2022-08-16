library(dplyr)
library(tidyverse)
library(jsonlite)
library(readxl)
library(stringr)

# sets the working directory. 
setwd("C:/Users/HUAWEI/Desktop/projects/The-Usual-Suspects")

# reads in the data. 
data <- read_csv("Data/combined data/combined_data.csv")

# transforms gdp per capita to its log value.
data$gdp_per_capita <- log(data$gdp_per_capita)

# checks each column for outliers and caps them at IQR * 1.5. 
cap_col_outliers <- function(column) {
  iqr <- IQR(data[,2][!is.na(data[,2])])
  upper_quantile <- quantile(data[,2][!is.na(data[,2])], 3/4, names=FALSE)
  lower_quantile <- quantile(data[,2][!is.na(data[,2])], 1/4, names=FALSE)
  
  high <- upper_quantile + (iqr*1.5)
  low <- lower_quantile - (iqr*1.5)
  
  outliers <- list()
  for(i in 1:nrow(column)) {
    value <- pull(column[i,])
    outliers[[i]] <- if(!is.na(value) & value > low & value < high) {
      value
    } else {
      if(!is.na(value) & value < low) {
        low
      } else {
        if(!is.na(value) & value > high) {
          high
        } else {
          NA
        }
      }
    }
  }
  return(unlist(outliers))
}

# returns a data frame of multiple capped outliers. 
cap_multiple_outliers <- function(data) {
  cols <- data[,-1]
  outlier_adjusted <- list()
  for(i in 1:ncol(cols)) {
    outlier_adjusted[[i]] <- tibble(value = cap_col_outliers(cols[,i]))
    colnames(outlier_adjusted[[i]]) <- colnames(cols[i])
  }
  outlier_adjusted <- outlier_adjusted %>%
    reduce(cbind)
  final_df <- data[,1] %>%
    cbind(outlier_adjusted)
  return(tibble(final_df))
}

# stores the capped outliers data frame.  
capped_data <- cap_multiple_outliers(data)


# changes certain columns to reflect that more of them is bad.
negative_values <- capped_data %>%
  mutate(infant_mortality = -infant_mortality,
         gini_index = -gini_index,
         poverty_1.90_USD = -poverty_1.90_USD,
         income_top_20_percent = -income_top_20_percent,
         slums = -slums)


# gets the z scores for individual columns. 
get_col_z_scores <- function(column) {
  name <- colnames(column)
  avg <- mean(pull(column[!is.na(column),]))
  std <- sd(pull(column[!is.na(column),]))
  
  z_scores <- list()
  for(i in 1:nrow(column)) {
    z_scores[[i]] <- (pull(column[i,]) - avg) / std
  }
  return(unlist(z_scores))
}

# returns a data frame of multiple z scores. 
get_multiple_z_scores <- function(data) {
  cols <- data[,-1]
  z_scores <- list()
  for(i in 1:ncol(cols)) {
    z_scores[[i]] <- tibble(value = get_col_z_scores(cols[,i]))
    colnames(z_scores[[i]]) <- colnames(cols[i])
  }
  z_scores <- z_scores %>%
    reduce(cbind)
  final_df <- data[,1] %>%
    cbind(z_scores)
  return(tibble(final_df))
}

# stores the z score transformed data frame. 
z_scores <- get_multiple_z_scores(negative_values)


# gets a new index measure by averaging column scores. 
create_index <- function(data, col_names) {
  measures <- data[,col_names]
  cols <- length(col_names)
  index <- list() 
  for(i in 1:nrow(measures)) {
    num <- sum(!is.na(measures[i,c(1:cols)]))
    index[[i]] <- if(sum(is.na(measures[i,c(1:cols)])) >= cols) {
      NA
    } else {
      sum(measures[i,which(!is.na(measures[i,c(1:cols)]))]) / num
    }
  }
  unlist(index)
}

# creates a list of index names. 
combinations <- c("technology", "politics", "education", "health", 
                  "happiness", "economic", "equality", "poverty")

# creates a data frame of index and column names. 
colnames <- tibble(technology = c("high_tech_exports", "rd_researchers", NA),
                   politics = c("corruption", "gov_effectivness", NA),
                   education = c("secondary_enrollment", "tertiary_enrollment", NA),
                   health = c("life_expectancy", "infant_mortality", NA),
                   happiness = c("positive_affect", "generosity", "freedom"),
                   economic = c("gdp_per_capita", NA, NA),
                   equality = c("gini_index", "income_top_20_percent", NA),
                   poverty = c("poverty_1.90_USD", "slums", NA))

# returns a data frame of multiple index values. 
create_multiple_indexes <- function(data) {
  indexes <- list()
  for(i in 1:length(combinations)) {
    cols <- pull(colnames[,combinations[i]][!is.na(pull(colnames[,combinations[i]])),])
    indexes[[i]] <- tibble(create_index(z_scores, cols))
    colnames(indexes[[i]]) <- combinations[[i]]
  }
  indexes <- indexes %>%
    reduce(cbind)
  final_df <- z_scores[,1] %>%
    cbind(indexes)
  return(tibble(final_df))
}

# stores the indexes data frame. 
indexes <- create_multiple_indexes(z_scores)


# gets the name of a NA absent column with the most variability. 
get_best_impute_var <- function(data) {
  cols <- data[-1]
  no_nas <- list() 
  for(i in 1:ncol(cols)) {
    no_nas[[i]] <- if(sum(is.na(cols[,i]))==0) i else NULL
  }
  no_nas <- unlist(no_nas)
  group_by_vars <- cols[,no_nas]
  
  best_variability <- list()
  for(i in 1:ncol(group_by_vars)) {
    best_variability[[i]] <- max(group_by_vars[,i]) - min(group_by_vars[,i])
  }
  best_variability <- first(which(unlist(best_variability) == max(unlist(best_variability))))
  impute_var <- colnames(group_by_vars[best_variability])
  return(impute_var)
}

# imputes missing values by taking a grouped average based on a complete 
# column we have full data for with the most variability.  
fill_NAs <- function(data, measure) {
  impute_var <- get_best_impute_var(data)
  
  # gets the grouped averages for missing values. 
  imputed_values <- data %>%
    mutate(impute_var = round(eval(parse(text=impute_var)))) %>%
    group_by(impute_var) %>%
    filter(!is.na(eval(parse(text=measure)))) %>%
    summarise(avg = mean(eval(parse(text=measure))))
  
  # imputes the missing values for the target column. 
  NAs <- data[is.na(data[,measure]),]
  imputations <- list()
  for(i in 1:nrow(NAs)) {
    imputations[[i]] <- imputed_values$avg[which(pull(imputed_values["impute_var"]) == pull(round(NAs[i,impute_var])))]
  }
  data[is.na(data[,measure]),measure] <- unlist(imputations)
  return(data[,measure])
}

# imputes multiple missing value columns. 
fill_multiple_NAs <- function(data) {
  na_cols <- list()
  for(i in 1:ncol(data)) {
    na_cols[[i]] <- if(sum(is.na(data[,i])>0)) TRUE else FALSE
  }
  na_cols <- colnames(data[,which(unlist(na_cols))])
  
  for(i in 1:length(na_cols)) {
    data[,na_cols[i]] <- fill_NAs(data, na_cols[i])
  }
  return(data)
}

# stores the cleaned data in a data frame. 
clean_data <- fill_multiple_NAs(indexes)

# saves the data frame as a csv file. 
write_csv(clean_data, "Data/combined data/clean_data.csv")
