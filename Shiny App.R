# Packages
library(tidyverse)
library(ggplot2)
library(sf)
library(leaflet)
library(stringr)
library(ggmap)
library(shiny)
library(shinythemes)
library(dplyr)

# Load data sets from the most recent quarter
#LRQ3_22 <- read_csv("2022-Q3_LRTravelTimes.csv")
#HRQ3_22 <- read_csv("2022-Q3_HRTravelTimes.csv")

# Sort by week
#LRQ3_22 <- LRQ3_22 %>% 
  #mutate(week = cut.Date(service_date, breaks = "1 week", labels = FALSE)) %>% 
  #arrange(service_date)
#HRQ3_22 <- HRQ3_22 %>% 
  #mutate(week = cut.Date(service_date, breaks = "1 week", labels = FALSE)) %>% 
  #arrange(service_date)

# Randomly selecting 2 weeks from each rail, and at least 3 pairs of stop
# week 1 and 2
#LQ3_22 <- LRQ3_22 %>%
  #filter(week == 2 | week == 5)
#LQ3_22 <- LQ3_22 %>%
  #filter(from_stop_id == 70110 | from_stop_id == 70126 | from_stop_id == 70182)
#LQ3_22 <- LQ3_22 %>%
  #filter(to_stop_id == 70112 | to_stop_id == 70144 | to_stop_id == 70152)

# week 3 and 4
#HQ3_22 <- HRQ3_22 %>%
  #filter(week == 6 | week == 8)
#HQ3_22 <- HQ3_22 %>%
  #filter(from_stop_id == 70001 | from_stop_id == 70015 | from_stop_id == 70071)
#HQ3_22 <- HQ3_22 %>%
  #filter(to_stop_id == 70077 | to_stop_id == 70097 | to_stop_id == 70279)

# Cleaning Data by binding rows
#df <- rbind(LQ3_22,HQ3_22)

# Shiny App
ui <- fluidPage(navbarPage("MBTA Travel Time", collapsible = TRUE, inverse = TRUE, theme = shinytheme("spacelab"),
                           tabPanel("Commuter Rail", 
                                    fluidPage(
                                      tabsetPanel(
                                        tabPanel("Stop Information",
                                                 mainPanel(
                                                   plotOutput("plot1"),
                                                   textOutput("text1"),
                                                   textOutput("text2"),
                                                   textOutput("text3"),
                                                   textOutput("text4"),
                                                   textOutput("text5"),
                                                   textOutput("text6"),
                                                   textOutput("text7"),
                                                   textOutput("text8"),
                                                   textOutput("text9"),
                                                   textOutput("text10"))
                                                   
                                        ),
                                        tabPanel("Direction Information",
                                                 mainPanel(
                                                   plotOutput("plot2"))
                                        ),
                                        tabPanel("Travel Time Information", 
                                                 mainPanel(
                                                   plotOutput("plot3"))))
                                      ))
                           
)
)


server <- function(input, output, session) {
  output$plot1 <- renderPlot({
    # Summarize data then plot1
    stp <- df %>% group_by(from_stop_id, to_stop_id) %>% summarise(`Average Travel Time` = mean(travel_time_sec))
    # Plot1
    ggp1 <- ggplot(stp,aes(x=as.factor(from_stop_id), y=`Average Travel Time`, fill=as.factor(to_stop_id)))
    ggp1 + geom_bar(position="dodge", stat="summary", fun="mean")
  })
  
  # Stop ID & Route Legend
  output$text1 <- renderText({
    "70001-70279 = (Orange)Forest Hills-Assembly"
  })
  
  output$text2 <- renderText({
    "70015-70279 = (Orange)Back Bay-Assembly"
  })
  
  output$text3 <- renderText({
    "70071-70077 = (Red)Kendall/MIT-Downtown Crossing"
  })
  
  output$text4 <- renderText({
    "70071-70097 = (Red+Orange)Kendall/MIT-Assembly"
  })
  
  output$text5 <- renderText({
    "70110-70112 = (Green-B)Park Street & North-Chestnut Hill Avenue"
  })
  
  output$text6 <- renderText({
    "70110-70144 = (Green-B)Park Street & North-Boston University Central"
  })
  
  output$text7 <- renderText({
    "70110-70152 = (Green-B/Green-C/Green-D)Park Street & North-Hynes Convention Center"
  })
  
  output$text8 <- renderText({
    "70126-70144 = (Green-B)Allston Street-Boston University Central"
  })
  
  output$text9 <- renderText({
    "70126-70152 = (Green-B)Allston Street-Hynes Convention Center"
  })
  
  output$text10 <- renderText({
    "70182-70152 = (Green-D)Longwood-Hynes Convention Center"
  })
 
  output$plot2 <- renderPlot({
    # Summarize data then plot2
    dir <- df %>% group_by(direction_id) %>% summarise(`Average Travel Time` = mean(travel_time_sec))
    # Plot2
    ggp2 <- ggplot(dir,aes(x=as.factor(direction_id), y=`Average Travel Time`, fill=as.factor(direction_id)))
    ggp2 + geom_bar(position="dodge", stat="summary", fun="mean")
  })
  
  output$plot3 <- renderPlot({
    # Summarize data then plot3
    avg <- df %>% group_by(route_id) %>% summarise(`Average Travel Time` = mean(travel_time_sec))
    # Plot3
    ggp3 <- ggplot(avg, aes(x=route_id, y=`Average Travel Time`, fill=route_id))
    ggp3 + geom_bar(position="dodge", stat="summary", fun="mean") + scale_fill_manual(values=c("#a1ba80",
                                                                                               "#386b43",
                                                                                               "orange",
                                                                                               "#F04550"))
  })
}

shinyApp(ui, server)
