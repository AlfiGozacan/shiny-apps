### Set working directory for local testing (comment out on publishing)

# setwd("C:/Users/agozacan/OneDrive - Humberside Fire and Rescue Service/Incidents Data/Apps/hotspots-test")

### Load libraries

library(shiny)
library(leaflet)
library(rgdal)
library(tidyverse)
library(lubridate)
library(RODBC)

### Define UI

ui <- fluidPage(
    
    # Application title
    titlePanel("Humberside Fire and Rescue Incidents"),
    
    sidebarLayout(
        
        sidebarPanel(
            
            # Incident type input
            selectInput(inputId = "inc_type",
                        label = h3("Select Incident Type"),
                        choices = list("FAMs" = "fa_malicious",
                                       "Special Services" = "special_service",
                                       "Secondary Fires" = "secondary_fire",
                                       "Primary Fires" = "primary_fire"),
                        selected = "fa_malicious"),
            
            # Date range input
            dateRangeInput("date_range", label = h3("Select Date Range"),
                           start = as.Date("2009-01-01","%Y-%m-%d"))
            
        ),
        
        mainPanel(
            
            # Display map
            leafletOutput("mymap"),
            
        )
        
    ),
    
    # Display time series
    plotOutput("timeseries")
    
)

### Define server logic

server <- function(input, output) {
    
    df <- reactive(
                read.csv(
                    paste(
                        input$inc_type,
                        ".csv",
                        sep = ""
                    )
                ) %>%
            filter((date(CREATION_DATE) > input$date_range[1]) &
                       (date(CREATION_DATE) < input$date_range[2])) %>%
            arrange(date(CREATION_DATE))
    )
    
    # Build map
    output$mymap <- renderLeaflet({
        
        map_df = df()
        
        coords.EPSG.27700 <- SpatialPoints(cbind(map_df$X, map_df$Y), proj4string=CRS("+init=epsg:27700"))
        coords.EPSG.4326 <- spTransform(coords.EPSG.27700, CRS("+init=epsg:4326"))
        
        leaflet() %>%
            addTiles() %>%
            addMarkers(data = as.data.frame(coords.EPSG.4326),
                       lng = ~coords.x1,
                       lat = ~coords.x2,
                       clusterOptions = markerClusterOptions(),
                       popup = paste("Incident Number:", map_df$INCIDENT_NUMBER, "<br>",
                                     "Creation Date:", map_df$CREATION_DATE, "<br>",
                                     "Incident Type:", map_df$INCIDENT_TYPE, "<br>",
                                     "Call Source:", map_df$CALL_SOURCE, "<br>",
                                     "UPRN:", map_df$GAZETTEER_URN
                       )
            )
        
    })
    
    output$timeseries <- renderPlot({
        
        timeseries_df = df()
        
        xlabels <- format(as.Date(timeseries_df$CREATION_DATE), "%Y-%m")
        xlabels <- unique(xlabels)
        xlabels[-seq(5, length(timeseries_df$CREATION_DATE), 5)] <- ""
        
        timeseries_df %>%
            mutate(month = format(as.Date(CREATION_DATE), "%Y-%m")) %>%
            group_by(month) %>%
            summarise(count = n()) %>%
            ggplot(aes(month, count, group=1)) +
            geom_line(colour="blue") +
            theme(text = element_text(size = 12,
                                      family = "mono"),
                  axis.text.x = element_text(angle = 70, vjust = 0.5)) +
            scale_x_discrete(labels=xlabels) +
            xlab("Month") +
            ylab("Frequency")
        
    })
    
}

### Run the application

shinyApp(ui = ui, server = server)
