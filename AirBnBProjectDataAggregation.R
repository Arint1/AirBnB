# *********************
# Data Aggregation File
# *********************

# Loading the libraries
library(tidyverse)
library(shiny)
library(lubridate)
library(ggthemes)
library(maps)
library(dplyr)
library(ggmap)
library(osmdata)
library(plotly)
library(ggrepel)
library(scales)
library(lattice)
library(DT)


# creating a saved new york city map called from google maps
nyc_map <- get_map( getbb('New York City'), source="stamen")

# calling the data set
NY_data <- read_csv("AB_NYC_2019.csv")
head(NY_data)
NY_data$price
NY_data$room_type


# saving the data set as its own file
save(NY_data, nyc_map,
     file = "2019NYCAirBnB_Aggregation.Rdata")