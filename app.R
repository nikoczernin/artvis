#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(stringr)
library(tidyverse)
library(shinyWidgets)
library(leaflet)

source("./choropleth.R")


# Define UI for application that draws a histogram
ui <- fluidPage(
  tags$head(
    # Add custom CSS to style the plot
    tags$style(HTML("
      .inline-plot {
        display: inline-block;
        width: 30%; /* Span 30% of the available space */
        vertical-align: top; /* Align plots inline at the top */
        margin-right: 1%; /* Add spacing between plots */
      }
    "))
  ),
  
  fluidRow(
    column(2,
            htmlOutput("variable_selection"),
            htmlOutput("artist_selection"),
            htmlOutput("country_selection"),
            htmlOutput("venue_selection"),
           ),
    column(10,
           fluidRow(
              plotOutput("vals_over_time"),
              htmlOutput("time_selection"),
              textOutput("display_period"),
              height="10%"
           ),
           fluidRow(
             leafletOutput("choropleth"),
             height="60%"),
           fluidRow(
              div( class = "inline-plot", plotOutput("top_k_artists")),
              div( class = "inline-plot", plotOutput("top_k_countries")),
              div( class = "inline-plot", plotOutput("top_k_venues")),
              height="30%"
           ),
         ),
  ),
)


# Define server logic required to draw a histogram
server <- function(input, output, session) {
  ns <- session$ns
  
  # read in the NEW preprocessed data
  artvis <- read.csv("artvis_dump_NEW.csv", sep = ";") %>% 
    # generate full name columns
    mutate(
      a.fullname = paste(a.firstname, a.lastname),
      a.fullname.rev = paste(a.lastname, a.firstname, sep=", ")
    )
  
  # Color Scheme
  color.active <- "orange"
  color.inactive <- "gray"
  
  
  # when grouping data over the different time periods, you need to 
  # get a set of all years in order
  first_year <- artvis$e.startdate %>% na.omit() %>% min() %>% as.numeric()  
  last_year  <- artvis$e.startdate %>% na.omit() %>% max() %>% as.numeric()  
  all_years <- seq(first_year, last_year)
    
  
  
  ##### Variable Selection #####
  output$variable_selection <- renderUI({
    selectizeInput(
      "var_of_interest",
      "Select a variable to count",
      choices = list("Number of exhibitions"="exhibitions", 
                     "Number of paintings"="paintings"),
    )
  })
  
  
  
  ##### Artist Selection #####
  output$artist_selection <- renderUI({
    # get all artist names, sorted
    all.artists <- artvis %>% 
      .$a.fullname %>% 
      unique() %>% 
      sort()
    # return the multiple choice pickerInput
    pickerInput(
      "selected_artist",
      "Select an artist",
      choices = all.artists,
      selected = all.artists, # start with preselection of all artists
      options = pickerOptions(
        actionsBox = TRUE, 
        size = 10,
        selectedTextFormat = "count > 3"
      ), 
      multiple = TRUE, # mulitple choice
    )
  })
  
  
  
  
  ##### Country Selection #####
  output$country_selection <- renderUI({
    # get all artist names, sorted
    all.countries <- artvis %>% 
      .$e.country %>% 
      unique() %>% 
      sort()
    # return the multiple choice pickerInput
    pickerInput(
      "selected_country",
      "Select a country",
      choices = all.countries,
      selected = all.countries, # start with preselection of all all.countries
      options = pickerOptions(
        actionsBox = TRUE, 
        size = 10,
        selectedTextFormat = "count > 3"
      ), 
      multiple = TRUE, # mulitple choice
    )
  })
  
  
  
  ##### Venue Selection #####
  output$venue_selection <- renderUI({
    # get all artist names, sorted
    all.venues <- artvis %>% 
      .$e.venue %>% 
      unique() %>% 
      sort()
    # return the multiple choice pickerInput
    pickerInput(
      "selected_venue",
      "Select a venue",
      choices = all.venues,
      selected = all.venues, # start with preselection of all all.venues
      options = pickerOptions(
        actionsBox = TRUE, 
        size = 10,
        selectedTextFormat = "count > 3"
      ), 
      multiple = TRUE, # mulitple choice
    )
  })
  
  
  
  
    

  ##### Timeline Picker #####
  output$time_selection <- renderUI({
    min_val = all_years %>% min()
    max_val = all_years %>% max()

    sliderTextInput("period",
                NULL,
                width="100%",
                grid = TRUE,
                animate = TRUE,
                selected = c(min_val, max_val),
                choices=seq(min_val, max_val)
    )
  })
  output$display_period <- renderText(input$period) # TODO: remove
  
  
  
  
  
  ##### Barplot above Timeline #####
  output$vals_over_time <- renderPlot({
    req(input$period)
    artvis %>% 
      group_by(e.startdate) %>% 
      summarise(n_exhibs = n_distinct(e.title)) %>% 
      mutate(year = e.startdate %>% as.numeric()) %>% 
      full_join(data.frame(year=all_years)) %>% 
      arrange(year) %>% 
      mutate(n_exhibs=ifelse(is.na(n_exhibs), 0, n_exhibs)) %>% 
      na.omit() %>% 
      ggplot(aes(x=year, y=n_exhibs, 
                 fill=ifelse(year %>% between(input$period[1], input$period[2]), color.active, color.inactive))) +
        geom_bar(stat = "identity") +
        theme_minimal() +
      scale_fill_identity() +
        theme(
          axis.title = element_blank(),
          axis.text = element_blank(),
          panel.grid = element_blank()
        )
    })
  
  
  
  ##### Choropleth Map ##### 
  output$choropleth <- renderLeaflet({
    # prepare the plot data (summarise)
    artvis %>%
      group_by(e.country) %>%
      summarise(n_exhibits = n_distinct(e.id)) %>%
      # pass it to the choropleth
      choropleth("n_exhibits", "e.country")
  })
  
  
  
  
  
  k <- 10
  ##### Barplot Top Artist #####
  output$top_k_artists <- renderPlot({
    
    artvis %>% 
      group_by(a.fullname) %>% 
      summarise(n_exhibs = n_distinct(e.title)) %>% 
      mutate(n_exhibs=ifelse(is.na(n_exhibs), 0, n_exhibs)) %>% 
      arrange(desc(n_exhibs)) %>% 
      head(k) %>% 
      ggplot(aes(x=fct_reorder(a.fullname, n_exhibs, .desc = T), y=n_exhibs, fill=n_exhibs)) +
        geom_bar(stat = "identity") +
        theme_minimal() +
        theme(
          axis.title = element_blank(),
          # axis.text = element_blank(),
          panel.grid = element_blank()
        )
    })
  
  
  ##### Barplot Top Countries #####
  output$top_k_countries <- renderPlot({
    
    artvis %>% 
      group_by(e.country) %>% 
      summarise(n_exhibs = n_distinct(e.title)) %>% 
      mutate(n_exhibs=ifelse(is.na(n_exhibs), 0, n_exhibs)) %>% 
      arrange(desc(n_exhibs)) %>% 
      head(k) %>% 
      ggplot(aes(x=fct_reorder(e.country, n_exhibs, .desc = T), y=n_exhibs, fill=n_exhibs)) +
        geom_bar(stat = "identity") +
        theme_minimal() +
        theme(
          axis.title = element_blank(),
          # axis.text = element_blank(),
          panel.grid = element_blank()
        )
    })
  
  
  # TODO: this code lets you get the width of the plot vals_over_time
  # right_col_width <- reactive(session$clientData[["output_vals_over_time_width"]])
  
  ##### Barplot Top Venues #####
  output$top_k_venues <- renderPlot({
    artvis %>% 
      group_by(e.venue) %>% 
      summarise(n_exhibs = n_distinct(e.title)) %>% 
      mutate(n_exhibs=ifelse(is.na(n_exhibs), 0, n_exhibs)) %>% 
      arrange(desc(n_exhibs)) %>% 
      head(k) %>% 
      ggplot(aes(x=fct_reorder(e.venue, n_exhibs, .desc = T), y=n_exhibs, fill=n_exhibs)) +
        geom_bar(stat = "identity") +
        theme_minimal() +
        theme(
          axis.title = element_blank(),
          # axis.text = element_blank(),
          panel.grid = element_blank()
        )
    })
  
  
    

}

# Run the application 
shinyApp(ui = ui, server = server)
