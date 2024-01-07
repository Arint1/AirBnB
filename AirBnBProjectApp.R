# ****************
# Project App File
# ****************

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
ui <- fluidPage(
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


### Define server behavior for application here
server <- function(input, output, session) {
  
  
  # output tab
  output$housetable <- DT::renderDataTable({
    
    # Filtering for the data to be affected by selected inputs
    NY_data_2 <- NY_data %>%
      select(id, name, host_id, host_name, neighbourhood_group, neighbourhood, room_type, price, minimum_nights, availability_365) %>%
      filter(
        price >= input$minPrice,
        price <= input$maxPrice,
        availability_365 <= input$available,
        neighbourhood_group %in% input$neighborhood,
        room_type %in% input$roomType,
        minimum_nights >= input$minNights)
    
    # Creating the dataTable to be plotted
    my.table <- DT::datatable(NY_data_2, class = 'cell-border stripe')
    my.table <- formatStyle(my.table,
                            columns = " ",
                            backgroundColor = "#008080",
                            borderBottomColor = "#dddddd",
                            borderBottomStyle = "solid",
                            borderBottomWidth = "1px",
                            borderCollapse = "collapse",
                            borderRightColor = "#dddddd",
                            borderRightStyle = "solid",
                            borderRightWidth = "1px",
                            color = "#dddddd",
                            fontFamily = "Arial")
    my.table
    
    
  })
  
  # Plot which creates the map
  output$mapplot <- renderPlot({
    
    # Filtering the data based on the selected inputs
    NY_data_1 <- filter(NY_data,
                        room_type == input$RoomType,
                        price >= input$prices[1],
                        price <= input$prices[2],
                        neighbourhood_group %in% input$neighbor, na.rm=TRUE)
    
    # Creating the map
    ggmap(nyc_map) +
      geom_point(aes(x=longitude, y=latitude), 
                 color = "red", shape=1, size =4,
                 data=NY_data_1) +
      theme_bw() +
      theme_map() +
      labs(title = "AirBnB Locations in New York City") 
    
  # Editing the height and weight of the map within the graph
  }, height = 600, width = 500, unit = "px")
  
  
  
  # The output for the Comparison plot
  output$compplot <- renderPlot({
    
    # Filtering the data based on the selected inputs
    NY_data_comp <- NY_data %>%
      filter( 
        room_type %in% input$selectRoomType,
        minimum_nights >= input$selectMinNights,
        neighbourhood %in% input$selectNeighbourhood1 | neighbourhood %in% input$selectNeighbourhood2)
    
    
    # Creating the side by side boxplots to show the data
    ggplot(NY_data_comp, aes(y = price, x = neighbourhood, color = neighbourhood)) +
      geom_boxplot(outlier.shape = NA) +
      geom_jitter(position = position_jitter(width = 0.05), alpha = 0.8) +
      #scale_color_manual(values= c("#FD1EFF", "#20FF1E", "#FF8D1E", "#1E90FF")) +
      theme_minimal() +
      labs(x= "Neighbourhoods", y="Price of AirBnBs") +
      theme(legend.title = element_blank(), 
            panel.grid.major = element_line(color = "white"),
            panel.grid.minor = element_line(color = "white")) +
      theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
      theme(legend.position = "none") +
      theme(legend.text = element_text(size = 12),
            legend.title = element_text(size = 15),
            axis.text.x = element_text(size = 15),
            axis.text.y = element_text(size = 15)) 
    
  })
  
}

### specify the ui and server objects to be combined to make App
shinyApp(ui=ui, server=server)