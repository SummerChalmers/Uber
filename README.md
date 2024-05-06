# Uber
In this assignment data from Uber rides from the months of May to September were given as asked for an analysis. The data included date and time in one column, lattitude, longtitude, and base. Specifically requested were: pivot table to display trips by the hour, chart that shows Trips by Hour and Month, chart that displays Trips Every Hour, plot data by trips taken during every day of the month, table that shows Trips Every Day, chart by Trips by Day and Month, chart Trips by Bases and Month, heat map that displays by hour and day, heat map by month and day, heat map by month and week, heat map bases and Day of Week. 

## Data Cleaning
```
#splitting Date.Time column
all_data$Date.Time <- as.POSIXct(all_data$Date.Time, format = "%m/%d/%Y %H:%M:%S")
all_data$Hour <- hour(all_data$Date.Time)
all_data$Date <- as.Date(all_data$Date.Time)
all_data$Month <- month(all_data$Date.Time, label = TRUE)
all_data$Day <- day(all_data$Date.Time)
all_data$Day_of_Week <- wday(all_data$Date.Time, label = TRUE)
```
## Code
Pivot Table by Hour
```
output$pivot_table_hour <- renderDataTable({
    pivot_table <- all_data %>%
      group_by(Hour) %>%
      summarise(Trips = n())
    pivot_table
```
Trips by Hour & Month
```
output$trips_by_hour_month <- renderPlot({
    ggplot(all_data, aes(x = Hour, fill = Month)) +
      geom_bar(position = "dodge") +
      labs(title = "Trips by Hour and Month", x = "Hour", y = "Trips", fill = "Month")
```

Trips Every Hour
```
output$trips_every_hour <- renderPlot({
    ggplot(all_data, aes(x = Hour)) +
      geom_histogram(binwidth = 1, fill = "#228b22", color = "black") +
      labs(title = "Trips Every Hour", x = "Hour", y = "Frequency")`
```

   Trips by Day of Month
   ```
   output$trips_by_day_of_month <- renderPlot({
    ggplot(all_data, aes(x = Day, y = ..count..)) +
      geom_bar(fill = '#228b22') +
      labs(title = "Trips by Day of Month", x = "Day", y = "Trips")
```
Trips Every Day- Pivot Table
```
output$trips_every_day <- renderDataTable({
    trips_every_day <- all_data %>%
      group_by(Day) %>%
      summarise(Trips = n())
    trips_every_day
```
Trips by Day and Month
```
output$trips_by_day_and_month <- renderPlot({
    ggplot(all_data, aes(x = Day_of_Week, fill = Month)) +
      geom_bar(position = "dodge") +
      labs(title = "Trips by Day and Month", x = "Day of Week", y = "Trips", fill = "Month")
```
Trips by Month
```
output$trips_by_month <- renderPlot({
    ggplot(all_data, aes(x = Month)) +
      geom_bar(fill = '#228b22') +
      labs(title = "Trips by Month", x = "Month", y = "Trips")
```
Trips by Bases and Month
```
output$trips_by_bases_and_month <- renderPlot({
    ggplot(all_data, aes(x = Base, fill = Month)) +
      geom_bar(position = "dodge") +
      labs(title = "Trips by Bases and Month", x = "Base", y = "Trips", fill = "Month")
```
## Heat Maps

Hour & Day
```
output$HM_hour_day <- renderPlot({
    hour_day_counts <- all_data %>%
      group_by(Hour, Day_of_Week) %>%
      summarise(Trips = n())
    
    ggplot(hour_day_counts, aes(x = Hour, y = Day_of_Week)) +
      geom_tile(aes(fill = Trips), colour = "white") +
      scale_fill_gradient(low = "white", high = "#228b22") +
      labs(title = "Heat Map on Hour and Day", x = "Hour", y = "Day of Week", fill = "Trips")
```
Month & Day
```
output$HM_month_day <- renderPlot({
    month_day_counts <- all_data %>%
      group_by(Month, Day) %>%
      summarise(Trips = n())
    
    ggplot(month_day_counts, aes(x = Day, y = Month)) +
      geom_tile(aes(fill = Trips), colour = "white") +
      scale_fill_gradient(low = "white", high = "#228b22") +
      labs(title = "Heat Map on Month and Day", x = "Day", y = "Month", fill = "Trips")
```
Month & Week
```
output$HM_month_week <- renderPlot({
    month_week_counts <- all_data %>%
      mutate(Week = as.numeric(format(Date, "%W"))) %>%
      group_by(Month, Week) %>%
      summarise(Trips = n())
    
    ggplot(month_week_counts, aes(x = Week, y = Month)) +
      geom_tile(aes(fill = Trips), colour = "white") +
      scale_fill_gradient(low = "white", high = "#228b22") +
      labs(title = "Heat Map on Month and Week", x = "Week", y = "Month", fill = "Trips")
```
Day of Week
```
output$HM_day_of_week <- renderPlot({
    base_day_counts <- all_data %>%
      group_by(Base, Day_of_Week) %>%
      summarise(Trips = n())
    
    ggplot(base_day_counts, aes(x = Base, y = Day_of_Week)) +
      geom_tile(aes(fill = Trips), colour = "white") +
      scale_fill_gradient(low = "white", high = "#228b22") +
      labs(title = "Heat Map on Bases and Day of Week", x = "Base", y = "Day of Week", fill = "Trips")
```

## Code to Render Into Shiny Dashboard
```
ui <- fluidPage(
  titlePanel("Rides Analysis for Uber"),
  sidebarLayout(
    sidebarPanel(
      dateRangeInput("date_range", "Date Range", start = min(all_data$Date), end = max(all_data$Date))
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Pivot Table - Trips by Hour",
                 dataTableOutput("pivot_table_hour")),
        tabPanel("Trips by Hour & Month",
                 plotOutput("trips_by_hour_month")),
        tabPanel("Trips Every Hour",
                 plotOutput("trips_every_hour")),
        tabPanel("Trips by Day of Month",
                 plotOutput("trips_by_day_of_month")),
        tabPanel("Trips Every Day",
                 dataTableOutput("trips_every_day")),
        tabPanel("Trips by Day and Month",
                 plotOutput("trips_by_day_and_month")),
        tabPanel("Trips by Month",
                 plotOutput("trips_by_month")),
        tabPanel("Trips by Bases & Month",
                 plotOutput("trips_by_bases_and_month")),
        tabPanel("Heat Maps",
                 tabsetPanel(
                   tabPanel("Heat Map by Hour & Day",
                            plotOutput("HM_hour_day")),
                   tabPanel("Heat Map by Month & Day",
                            plotOutput("HM_month_day")),
                   tabPanel("Heat Map by Month & Week",
                            plotOutput("HM_month_week")),
                   tabPanel("Heat Map by Bases & Day of Week",
                            plotOutput("HM_day_of_week"))
                 )
        )
      )
    )
  )
)
```
