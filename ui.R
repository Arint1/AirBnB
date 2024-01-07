
#Calling the data from the Aggregation File
load("2019NYCAirBnB_Aggregation.Rdata")

#Loading Libraries
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


# two choice vectors to be used in the application
choices <- c("Private room", "Entire home/apt", "Shared room")
nchoices <- c("Brooklyn", "Manhattan", "Queens", "Staten Island", "Bronx")



### Define UI for application
fluidPage(
  # Application title
  titlePanel(title="AirBnB Data 2019"),
  sidebarLayout(
    
    # Sidebar Panel
    sidebarPanel(
      # Conditional Panel for the Maps tab
      conditionalPanel(
        condition = "input.tabs == 'Maps'",
        selectInput(inputId = "RoomType",
                    label="Select Type of Room",
                    choices=choices),
        
        sliderInput(inputId = "prices",
                    label="Choose the Price of houses: ",
                    min=0, max=800, value=c(100, 700)),
        checkboxGroupInput(inputId = "neighbor",
                           label="Neighborhood Groups",
                           choices = nchoices,
                           selected = nchoices),
        
      ),
      # Conditional Panel for the Housing Information tab
      conditionalPanel(
        condition = "input.tabs == 'Housing Information'",
        #select the neighbourhood groups
        selectInput("neighborhood", 
                    "Neighbourhood Groups",
                    selected = nchoices,
                    choices = nchoices,
                    multiple = TRUE),
        # select the number of rooms
        selectInput("roomType",
                    "Room Type",
                    choices = choices,
                    selected = choices,
                    multiple = TRUE),
        # select the minimum price for the AirBnB
        numericInput("minPrice", 
                     "Min Price", 
                     min=0, 
                     max = 800, 
                     value=0),
        # select the max price for the AirBnB
        numericInput("maxPrice", 
                     "Max Price", 
                     min=0, 
                     max=800, 
                     value = 800),
        #select the number of nights
        numericInput("minNights",
                     "Minimum Nights to Stay",
                     min=0,
                     max=365,
                     value = 0),
        # select the amount of days available
        numericInput("available", 
                     "Availablilty by Days", 
                     min=0, 
                     max=365, 
                     value = 365)
      ),
      
      #Conditional Panel for the AirBnB Comparison Tab 
      
      conditionalPanel(
        condition = "input.tabs == 'AirBnB Neighbourhood Comparison'",
        #select which neighbourhoods to plot
        selectizeInput(inputId = "selectNeighbourhood1",
                       label = "Select Neighbourhoods (Max 4)",
                       choices = unique(NY_data$neighbourhood),
                       multiple = TRUE,
                       options = list(maxItems = 4, placeholder = 'Enter Neighbourhood Name',
                                      onInitialize = I('function() { this.setValue(""); }'))
        ),
        #select which rooms to plot
        selectInput(inputId = "selectRoomType",
                    label = "Select Room Type",
                    choices = choices),
        #select the number of nights to plot
        numericInput(inputId = "selectMinNights",
                     label = "Select the Minimum Number of Nights",
                     min = 0,
                     max = 365,
                     value = 0)
      )
    ),  
    
    # Main panel calling three different tabPanels that each have
    # there own Output
    
    mainPanel(
      tabsetPanel(id = "tabs",
                  tabPanel("Maps", plotOutput("mapplot"), height = "100vh"),
                  tabPanel("Housing Information", DT::dataTableOutput("housetable")),
                  tabPanel("AirBnB Neighbourhood Comparison", plotOutput("compplot"))
      )
    )
  )
)