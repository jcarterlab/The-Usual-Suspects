library(dplyr)
library(tidyverse)
library(stringr)
library(ggthemes)
library(stats)
library(factoextra)

# sets the seed for random number generation. 
set.seed(1995)

# sets max overlaps in GGplot to infinity. 
options(ggrepel.max.overlaps = Inf)

# my personal plot theme for data visualizations. 
my_theme <- theme_economist_white(gray_bg = FALSE) +
  theme(plot.title = element_text(hjust = 0.5,
                                  vjust = 10,
                                  size = 10,
                                  color = "#474747"),
        plot.margin = unit(c(1.5, 1, 1.5, 1), "cm"),
        axis.text = element_text(size = 9,
                                 color = "gray30"),
        axis.text.x=element_text(vjust = -2.5),
        axis.title.x = element_text(size = 9,
                                    color = "gray30",
                                    vjust = -10),
        axis.title.y = element_text(size = 9,
                                    color = "gray30",
                                    vjust = 10),
        legend.direction = "vertical", 
        legend.position = "right",
        legend.title = element_blank(),
        legend.text = element_text(size = 11,
                                   color = "gray20"),
        legend.margin=margin(1, -15, 1, 0),
        legend.spacing.x = unit(0.25, "cm"),
        legend.key.size = unit(1, "cm"), 
        legend.key.height = unit(0.75, "cm"),
        strip.text = element_text(hjust = 0.5,
                                  vjust = 1,
                                  size = 10,
                                  color = "#474747"),
        panel.spacing = unit(2, "lines"))

# sets the working directory. 
setwd("C:/Users/HUAWEI/Desktop/Exploratory data/The Magnificent Seven")

# reads in the data. 
data <- read_csv("Data/combined data/clean_data.csv")

# sets a countries variable. 
countries <- tibble(countries = data$country)

# creates a data frame of country name reassignments. 
reassignments <- tibble(original = c("Bosnia and Herzegovina",
                                     "Congo, Dem. Rep.",
                                     "Congo, Rep.",
                                     "Egypt, Arab Rep.",
                                     "Gambia, The",
                                     "Korea, Rep.",
                                     "Kyrgyz Republic",
                                     "Lao PDR",
                                     "North Macedonia",
                                     "Russian Federation",
                                     "Sao Tome and Principe",
                                     "Slovak Republic",
                                     "South Sudan",
                                     "Syrian Arab Republic",
                                     "United Arab Emirates",
                                     "United Kingdom",
                                     "United States",
                                     "Venezuela, RB",
                                     "West Bank and Gaza",
                                     "Yemen, Rep.",
                                     "Hong Kong SAR, China",
                                     "Iran, Islamic Rep."),
                        new = c("Bosnia",
                                "DRC",
                                "Congo",
                                "Egypt",
                                "Gambia",
                                "S. Korea",
                                "Kyrgyzstan",
                                "Laos",
                                "N. Macedonia",
                                "Russia",
                                "Sao Tome",
                                "Slovakia",
                                "S. Sudan",
                                "Syria",
                                "UAE",
                                "UK",
                                "US",
                                "Venezuela",
                                "Palestine",
                                "Yemen",
                                "Hong Kong",
                                "Iran"))

# reassigns the country names for all data frames. 
reassign_countries <- function(data) {
  countries <- data
  for(i in 1:length(countries)) {
    countries[i] <- if(countries[i] %in% reassignments$original) {
      reassignments$new[which(countries[i]==reassignments$original)]
    }
    else {
      countries[i]
    }
  }
  return(countries)
}

# reassigns all countries in all data frames. 
for(i in 1:length(countries)) {
  countries$countries <- reassign_countries(countries$countries)
}

# takes the countries column away from the data frame. 
data <- data[-1]

# sets the row names to the country names. 
rownames(data) <- countries$countries

# creates a k-means object. 
kmeans.res <- kmeans(data, centers=3, iter.max = 10, nstart = 1)

# creates a list of colors. 
green <- which(table(kmeans.res$cluster)==42)
orange <- which(table(kmeans.res$cluster)==75)
red <- which(table(kmeans.res$cluster)==48)

col_values <- c("#009900", "#FF8000", "#CC0000")
cols <- c(col_values[green], col_values[orange], col_values[red])

# visualizes the data with the factoextra package. 
fviz_cluster(object=kmeans.res,
             data=data,
             stand=FALSE,
             geom = "text",
             repel=TRUE,
             ellipse.type = "t",
             ellipse.level=0.95,
             ellipse.alpha = 0.1,
             labelsize = 8) +
  scale_colour_manual(values = cols) +
  scale_fill_manual(values = cols) + 
  ggtitle("Development") +
  my_theme +
  theme(legend.position="none")



