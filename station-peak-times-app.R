### Load libraries

library(shiny)
library(tidyverse)
library(lubridate)
library(RODBC)
library(plotly)

### Define station names

choices = list("Barton" = "BARTON",
               "Beverley" = "BEVERLEY",
               "Bransholme" = "BRANSHOLME",
               "Bridlington" = "BRIDLINGTON",
               "Brigg"= "BRIGG",
               "Brough" = "BROUGH",
               "Calvert Lane" = "CALVERT LANE",
               "Cleethorpes" = "CLEETHORPES",
               "Clough Road" = "CLOUGH ROAD",
               "Driffield" = "DRIFFIELD",
               "East Hull" = "EAST HULL",
               "Epworth" = "EPWORTH",
               "Goole" = "GOOLE",
               "Hornsea" = "HORNSEA",
               "Howden" = "HOWDEN",
               "Hull Central" = "HULL CENTRAL",
               "Immingham East" = "IMMINGHAM EAST",
               "Kirton in Lindsey" = "KIRTON IN LINDSEY",
               "Market Weighton" = "MARKET WEIGHTON",
               "Patrington" = "PATRINGTON",
               "Peaks Lane" = "PEAKS LANE",
               "Pocklington" = "POCKLINGTON",
               "Preston" = "PRESTON",
               "Scunthorpe" = "SCUNTHORPE",
               "Service HQ" = "SERVICE HQ",
               "Snaith" = "SNAITH",
               "Waltham" = "WALTHAM",
               "Winterton" = "WINTERTON",
               "Withernsea" = "WITHERNSEA")

all_stations = unlist(choices, use.names=FALSE)

### Define UI

ui <- fluidPage(
    
    # Application title
    titlePanel("Humberside Fire and Rescue: Station Peak Times"),
        
    sidebarPanel(
        
        width=2,
        
        # Toggle between incident count at instant of creation and time period engaged
        radioButtons(
            "toggle1",
            label = h3("Toggle Between Graph Type"),
            choices = list(
                "Incident count by hour band" = "ioc",
                "Average incident length by hour band"
            ),
            selected = "ioc"
        ),
        
        # Date range input
        dateRangeInput("date_range1", label = h3("Select Date Range"),
                       start = as.Date("2017-01-01","%Y-%m-%d")),
        
        # Select all button
        actionButton("selectall1", "Select/Deselect All Stations"),
        
        # Incident type input
        checkboxGroupInput(inputId = "stations1",
                    label = h3("Select Station(s)"),
                    choices = choices,
                    selected = "BARTON")
        
    ),
    
    mainPanel(
        
        p("A non-zero deviation indicates more or fewer incidents than expected, based on the average proportions of incidents in each hour band (where the average is calculated such that each station carries the same weight). If the error 'problem with `mutate()` column `deviation`' occurs, please extend date range."),
        
        width=8,
        
        # Display histogram 1
        plotlyOutput("histogram1"),
        
        #Display histogram 2
        plotlyOutput("histogram2")
        
    ),
    
    sidebarPanel(
        
        width=2,
        
        # Toggle between incident count at instant of creation and time period engaged
        radioButtons(
            "toggle2",
             label = h3("Toggle Between Graph Type"),
             choices = list(
                "Incident count by hour band" = "ioc",
                "Average incident length by hour band"
             ),
             selected = "ioc"
        ),
        
        # Date range input
        dateRangeInput("date_range2", label = h3("Select Date Range"),
                       start = as.Date("2017-01-01","%Y-%m-%d")),
        
        # Select all button
        actionButton("selectall2", "Select/Deselect All Stations"),
        
        # Incident type input
        checkboxGroupInput(inputId = "stations2",
                           label = h3("Select Station(s)"),
                           choices = choices,
                           selected = "BEVERLEY")
        
    )
    
)

### Define server logic

server <- function(input, output) {
    
    # Connect to SQL server
    dbhandle <- odbcDriverConnect("driver={SQL Server};server=HQIRS;database=threetc_irs;trusted_connection=true")
    
    # Write SQL query to save to df
    SQL_table <- sqlQuery(dbhandle,
                   "
                    select inc_incident.inc_incident_ref,
                           inc_incident.inc_date_created,
                           inc_incident.inc_time_of_call,
                           inc_incident.inc_time_stopped_mobilising,
                           inc_incident.inc_time_closed,
                           inc_incident.inc_initial_incident_type,
                           inc_incident.inc_location_coord_easting,
                           inc_incident.inc_location_coord_northing,
                           sta_station.sta_name
                    from inc_incident
                    join sta_station
                    on inc_incident.inc_responsible_station_id = sta_station.sta_pk
                    where inc_date_created > '2017-01-01'
                    "
    )
    
    # Remove old stations (where there is at least one hour band without incident) from the table
    old_stations = c("CROMWELL RD", "CROWLE", "IMMINGHAM WEST", "SLEDMERE")
    
    SQL_table <- SQL_table[-which(SQL_table$sta_name %in% old_stations),]
    
    # Make first reactive dataframe
    df1 <- reactive(
        SQL_table %>%
            filter(sta_name %in% input$stations1) %>%
            filter((date(inc_date_created) > input$date_range1[1]) &
                       (date(inc_date_created) < input$date_range1[2])) %>%
            arrange(date(inc_date_created))
    )
    
    # Make second reactive dataframe
    df2 <- reactive(
        SQL_table %>%
            filter(sta_name %in% input$stations2) %>%
            filter((date(inc_date_created) > input$date_range2[1]) &
                       (date(inc_date_created) < input$date_range2[2])) %>%
            arrange(date(inc_date_created))
    )
    
    # Facilitate first select all button
    observe({
        
        if (input$selectall1 > 0) {
            
            if (input$selectall1 %% 2 == 1) {
                
                updateCheckboxGroupInput(inputId="stations1",
                                         choices=choices,
                                         selected=all_stations)
                
            } else {
                
                updateCheckboxGroupInput(inputId="stations1",
                                         choices=choices,
                                         selected=c())
                
            }
        }
    })
    
    # Facilitate second select all button
    observe({
        
        if (input$selectall2 > 0) {
            
            if (input$selectall2 %% 2 == 1) {
                
                updateCheckboxGroupInput(inputId="stations2",
                                         choices=choices,
                                         selected=all_stations)
                
            } else {
                
                updateCheckboxGroupInput(inputId="stations2",
                                         choices=choices,
                                         selected=c())
                
            }
        }
    })
    
    # Make vectors of average proportions and standard deviations for all hour bands
    proportions = matrix(0, nrow=24, ncol=length(all_stations))
    
    for(i in 1:24){
        
        for(j in 1:length(all_stations)){
            
            props = SQL_table %>%
                filter(sta_name == all_stations[j]) %>%
                mutate(inc_time_created = format(inc_date_created, "%H:%M:%S")) %>%
                mutate(inc_seconds_since_midnight = plyr::round_any(
                    period_to_seconds(hms(inc_time_created)) - period_to_seconds(hms("00:00:00")), 3600, f=ceiling)) %>%
                count(inc_seconds_since_midnight) %>%
                mutate(proportion = n/sum(n)) %>%
                select(proportion) %>%
                .[[1]]
            
            proportions[,j] = props
            
        }
        
    }
    
    avg_proportions = rowMeans(proportions)
    
    sd_proportions = c()
    
    for(i in 1:24){
        
        sd_proportions[i] = sd(proportions[i,])
        
    }
    
    # Make first histogram
    output$histogram1 <- renderPlotly({
        
        hist_df = df1()
        
        if (input$toggle1 == "ioc") {
        
            p = hist_df %>%
                mutate(inc_time_created = format(inc_date_created, "%H:%M:%S")) %>%
                mutate(inc_seconds_since_midnight = plyr::round_any(
                    period_to_seconds(hms(inc_time_created)) - period_to_seconds(hms("00:00:00")), 3600, f=ceiling)) %>%
                count(inc_seconds_since_midnight) %>%
                mutate(hour_band = inc_seconds_since_midnight/3600) %>%
                mutate(proportion = n / sum(n)) %>%
                rename(frequency = n) %>%
                mutate(deviation = (proportion - avg_proportions)/sd_proportions) %>%
                ggplot(aes(hour_band, frequency)) +
                geom_col(aes(fill=deviation)) +
                scale_fill_gradient(name = "Deviation") +
                xlab("Hour Band") +
                ylab("Frequency")
            
        } else {
            
            p = hist_df %>%
                mutate(inc_time_created = format(inc_date_created, "%H:%M:%S")) %>%
                mutate(inc_start_since_midnight = plyr::round_any(
                    period_to_seconds(hms(inc_time_created)) - period_to_seconds(hms("00:00:00")), 3600, f=ceiling)) %>%
                mutate(
                    inc_length_of_inc = time_length(interval(
                        ymd_hms(inc_time_of_call),
                        ymd_hms(inc_time_closed)),
                        "second")) %>%
                filter(inc_length_of_inc > 0) %>%
                group_by(inc_start_since_midnight) %>%
                summarise(n = n(), avg_incident_length_mins = mean(inc_length_of_inc)/60) %>%
                mutate(hour_band = inc_start_since_midnight/3600) %>%
                rename(frequency = n) %>%
                ggplot(aes(hour_band, frequency)) +
                geom_col(aes(fill=avg_incident_length_mins)) +
                scale_fill_gradient(low="red4", high="red", name = "Avg Incident Length (mins)") +
                xlab("Hour Band") +
                ylab("Frequency")
            
        }
        
        ggplotly(p)
        
    })
    
    # Make second histogram
    output$histogram2 <- renderPlotly({
        
        hist_df = df2()
        
        if (input$toggle2 == "ioc") {
        
            p = hist_df %>%
                mutate(inc_time_created = format(inc_date_created, "%H:%M:%S")) %>%
                mutate(inc_seconds_since_midnight = plyr::round_any(
                    period_to_seconds(hms(inc_time_created)) - period_to_seconds(hms("00:00:00")), 3600, f=ceiling)) %>%
                count(inc_seconds_since_midnight) %>%
                mutate(hour_band = inc_seconds_since_midnight/3600) %>%
                mutate(proportion = n / sum(n)) %>%
                rename(frequency = n) %>%
                mutate(deviation = (proportion - avg_proportions)/sd_proportions) %>%
                ggplot(aes(hour_band, frequency)) +
                geom_col(aes(fill=deviation)) +
                scale_fill_gradient(name = "Deviation") +
                xlab("Hour Band") +
                ylab("Frequency")
            
        } else {
            
            p = hist_df %>%
                mutate(inc_time_created = format(inc_date_created, "%H:%M:%S")) %>%
                mutate(inc_start_since_midnight = plyr::round_any(
                    period_to_seconds(hms(inc_time_created)) - period_to_seconds(hms("00:00:00")), 3600, f=ceiling)) %>%
                mutate(
                    inc_length_of_inc = time_length(interval(
                        ymd_hms(inc_time_of_call),
                        ymd_hms(inc_time_closed)),
                        "second")) %>%
                filter(inc_length_of_inc > 0) %>%
                group_by(inc_start_since_midnight) %>%
                summarise(n = n(), avg_incident_length_mins = mean(inc_length_of_inc)/60) %>%
                mutate(hour_band = inc_start_since_midnight/3600) %>%
                rename(frequency = n) %>%
                ggplot(aes(hour_band, frequency)) +
                geom_col(aes(fill=avg_incident_length_mins)) +
                scale_fill_gradient(low="red4", high="red", name = "Avg Incident Length (mins)") +
                xlab("Hour Band") +
                ylab("Frequency")
            
        }
        
        ggplotly(p)
        
    })
    
}

### Run the application

shinyApp(ui = ui, server = server)
