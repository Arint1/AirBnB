#Calling the data from the Aggregation File
load("2019NYCAirBnB_Aggregation.Rdata")

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