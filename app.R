# Author: [Your Name]
# Date: [Today's Date]
# Description: A Shiny app to visualize art exhibition data. 
#              Includes interactive choropleth maps, bar plots, and filter options.


# Required Libraries
library(shiny)         # Core shiny library for building the app
library(stringr)       # String manipulation functions
library(tidyverse)     # Data manipulation and visualization
library(shinyWidgets)  # Additional UI widgets for Shiny
library(leaflet)       # Interactive maps

# Source external helper functions for choropleth map
source("./choropleth.R")


##### User Interface (UI) for the Shiny App #####
ui <- fluidPage(
  tags$head(
    # Custom CSS for inline plots
    tags$style(HTML("
      
      /* 30% with inline barplots */
      .inline-plot {
        display: inline-block;
        width: 30%; /* Span 30% of the available space */
        vertical-align: top; /* Align plots inline at the top */
        margin-right: 1%; /* Add spacing between plots */
      }
      
      
      /* Allow for vertical scrolling */
      .scrollBox {
        overflow-y: scroll;
      }
      
      /* TRANSPARENT */
      .transparent {
        opacity: 0.5;
      }
      
    "))
  ),
  
  
  # Layout with two columns
  fluidRow(
    textOutput("display"),   # Display for debugging
    # Left column for filter selection
    column(2,
           htmlOutput("variable_selection"),  # Dropdown for selecting the variable to count
           # Multi-select for selecting artists
           div(
             htmlOutput("artist_selection"),
             style='height:25vh',
             class = "scrollBox",
            ),
           div(
             htmlOutput("country_selection"),   # Multi-select for selecting countries
             style='height:25vh',
             class = "scrollBox",
            ),
           div(
             htmlOutput("venue_selection"),      # Multi-select for selecting venues
             style='height:25vh',
             class = "scrollBox",
            ),
    ),
    # Right column for visualizations
    column(10,
           fluidRow(
             plotOutput("vals_over_time", height = "100px"), # Bar plot for values over time
             htmlOutput("time_selection"),   # Slider input for selecting time range
             height = "10%"
           ),
           fluidRow(
             # leafletOutput("choropleth"),    # Interactive choropleth map
             height="60%"),
           fluidRow(
             # Three inline bar plots for top artists, countries, and venues
             div(class = "inline-plot", plotOutput("top_k_artists")),
             div(class = "inline-plot", plotOutput("top_k_countries")),
             div(class = "inline-plot", plotOutput("top_k_venues")),
             height = "30%"
           ),
         ),
  ),
)




##### Server Logic for the Shiny App #####
server <- function(input, output, session) {
  ns <- session$ns  # Namespace for input/output elements
  
  # Load and preprocess the art exhibition data
  artvis <- read.csv("./data/artvis_dump_NEW.csv", sep = ";") %>% 
    # generate full name columns
    mutate(
      a.fullname = paste(a.firstname, a.lastname),
      a.fullname.rev = paste(a.lastname, a.firstname, sep=", "),
      # i think its wise to always use lastname first
      a.fullname = a.fullname.rev
    )
  
  # Define colors for active and inactive data points
  color.active <- "orange"
  color.inactive <- "gray"
  
  
  # Extract the range of years from the dataset
  first_year <- artvis$e.startdate %>% na.omit() %>% min() %>% as.numeric()  
  last_year  <- artvis$e.startdate %>% na.omit() %>% max() %>% as.numeric()  
  all_years <- seq(first_year, last_year)
    
  
  
  ##### Variable Selection #####
  # Dropdown for choosing a variable to count
  output$variable_selection <- renderUI({
    selectizeInput(
      inputId = "var_of_interest",
      label = "Select a variable to count",
      choices = list("Number of exhibitions"="exhibitions", 
                     "Number of paintings"="paintings"),
    )
  })
  
  
  ##### Get aggregates of selected variable #####
  #### Get artist aggregates
  artist.totals <- reactive({
    req(input$var_of_interest)
    req(input$period)
    # aggregate the selected variable per artist
    artvis %>% 
      # apply the date filter
      filter(e.startdate %>% between(input$period[1], input$period[2])) %>% 
      group_by(a.fullname) %>% 
      summarise(n = n_distinct(e.id)) %>% 
      # now join it back with all artists so that filtered out artists
      # get a zero
      right_join(artvis %>% select(a.fullname) %>% unique()) %>% 
      mutate(n = ifelse(is.na(n), 0, n)) %>% 
      arrange(a.fullname) %>% 
      .$n
  })
  
  
  #### Get country aggregates
  country.totals <- reactive({
    req(input$var_of_interest)
    req(input$period)
    # aggregate the selected variable per artist
    artvis %>% 
      # apply the date filter
      filter(e.startdate %>% between(input$period[1], input$period[2])) %>% 
      group_by(e.country) %>% 
      summarise(n = n_distinct(e.id)) %>% 
      # now join it back with all artists so that filtered out artists
      # get a zero
      right_join(artvis %>% select(e.country) %>% unique()) %>% 
      mutate(n = ifelse(is.na(n), 0, n)) %>% 
      arrange(e.country) %>% 
      .$n
  })
  
  
  #### Get venue aggregates
  venue.totals <- reactive({
    req(input$var_of_interest)
    req(input$period)
    # aggregate the selected variable per artist
    artvis %>% 
      # apply the date filter
      filter(e.startdate %>% between(input$period[1], input$period[2])) %>% 
      group_by(e.venue) %>% 
      summarise(n = n_distinct(e.id)) %>% 
      # now join it back with all artists so that filtered out artists
      # get a zero
      right_join(artvis %>% select(e.venue) %>% unique()) %>% 
      mutate(n = ifelse(is.na(n), 0, n)) %>% 
      arrange(e.venue) %>% 
      .$n
  })
  
  
  
  
  
  ##### Get unique category values #####
  
  # get all artist names, sorted
  all.artists <- artvis %>% 
    .$a.fullname %>% 
    unique() %>% 
    sort() %>% 
    .[.!=""] # remove empty strings
  
  # get all country names, sorted
  all.countries <- artvis %>% 
    .$e.country %>% 
    unique() %>% 
    sort() %>% 
    .[.!=""] # remove empty strings
  
  # get all venue names, sorted
  all.venues <- artvis %>% 
    .$e.venue %>% 
    unique() %>% 
    sort() %>% 
    .[.!=""] # remove empty strings

  

    
  ##### Multiple Choice Filter Widgets #####
  
  multiple_selection_container <- function(intputId, label="", options, values=NULL){
    # options: vector of option values
    # prefix: prefix of input variable name
    renderUI({
      # prepare the classes
      # if no values have been passed, just use empty values (not 0 though)
      if (is.null(values)) values <- function() rep("", length(options))
      # to any values==0 we add the "transparent" class
      classes <- sapply(values(), function(v) ifelse(v == 0, "transparent", ""))
      checkboxGroupInput(
        inputId = intputId,
        label = label,
        # for the choice labels, iterate over the indices of all options
        choiceNames = seq(length(options)) %>% 
          # add the ith choice and give it the ith class
          lapply(function(i) tags$div(options[i], class = classes[i])),
        choiceValues = options,
        selected = options # preselect everything
      )
    })
  }
  
  
  ##### Artist Selection Widget #####
  output$artist_selection <- multiple_selection_container(
    intputId = "artist_selection",
    label = "Select artists",
    options = all.artists,
    values = artist.totals
  )
  
  ##### Country Selection Widget #####
  output$country_selection <- multiple_selection_container(
    intputId = "country_selection",
    label = "Select countries",
    options = all.countries,
    values = country.totals
  )

  ##### Venue Selection Widget #####
  output$venue_selection <- multiple_selection_container(
    intputId = "venue_selection",
    label = "Select venues",
    options = all.venues,
    values = venue.totals
  )
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ##### Country Selection Widget #####
  # Multi-select dropdown for choosing countries
  # output$country_selection <- renderUI({
  #   # get all artist names, sorted
  #   # return the multiple choice pickerInput
  #   pickerInput(
  #     "selected_country",
  #     "Select a country",
  #     choices = all.countries,
  #     selected = all.countries, # start with preselection of all all.countries
  #     options = pickerOptions(
  #       actionsBox = TRUE, 
  #       size = 10,
  #       selectedTextFormat = "count > 3"
  #     ), 
  #     multiple = TRUE, # mulitple choice
  #   )
  # })
  
  
  
  ##### Venue Selection Widget #####
  # # Multi-select dropdown for choosing venues
  # output$venue_selection <- renderUI({
  #   # return the multiple choice pickerInput
  #   pickerInput(
  #     "selected_venue",
  #     "Select a venue",
  #     choices = all.venues,
  #     selected = all.venues, # start with preselection of all all.venues
  #     options = pickerOptions(
  #       actionsBox = TRUE, 
  #       size = 10,
  #       selectedTextFormat = "count > 3"
  #     ), 
  #     multiple = TRUE, # mulitple choice
  #   )
  # })
  # 
  
  
  
    

  ##### Timeline Slider #####
  # Slider input for selecting a time range
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
  
  
  
  
  
  ##### Barplot: Exhibitions Over Time #####
  # Bar plot showing the number of exhibitions per year
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
                 fill=ifelse(year %>% between(input$period[1], input$period[2]), 
                             color.active, color.inactive))) +
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
  # Render an interactive choropleth map based on the filtered data
  output$choropleth <- renderLeaflet({
    req(input$selected_artist)
    req(input$selected_country)
    req(input$selected_venue)
    req(input$period)
    
    # prepare the plot data (summarise)
    artvis %>%
      # apply the filters
      filter(e.startdate %>% between(input$period[1], input$period[2])) %>% 
      filter(a.fullname %in% input$selected_artist) %>% 
      filter(e.country %in% input$selected_country) %>% 
      filter(e.venue %in% input$selected_venue) %>% 
      group_by(e.country) %>%
      summarise(n_exhibits = n_distinct(e.id)) %>%
      # pass it to the choropleth
      choropleth("n_exhibits", "e.country")
  })
  
  
  
  
  
  k <- 10
  ##### Top Artists Barplot #####
  output$top_k_artists <- renderPlot({
    req(input$selected_artist)
    req(input$selected_country)
    req(input$selected_venue)
    req(input$period)

    artvis %>% 
      # apply the filters
      filter(e.startdate %>% between(input$period[1], input$period[2])) %>% 
      filter(a.fullname %in% input$selected_artist) %>% 
      filter(e.country %in% input$selected_country) %>% 
      filter(e.venue %in% input$selected_venue) %>% 
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
    req(input$selected_artist)
    req(input$selected_country)
    req(input$selected_venue)
    req(input$period)
    
    artvis %>% 
      # apply filters
      # apply the filters
      filter(e.startdate %>% between(input$period[1], input$period[2])) %>% 
      filter(a.fullname %in% input$selected_artist) %>% 
      filter(e.country %in% input$selected_country) %>% 
      filter(e.venue %in% input$selected_venue) %>% 
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
  
  
  ##### Barplot Top Venues #####
  output$top_k_venues <- renderPlot({
    req(input$selected_artist)
    req(input$selected_country)
    req(input$selected_venue)
    req(input$period)
    
    artvis %>% 
      # apply the filters
      filter(e.startdate %>% between(input$period[1], input$period[2])) %>% 
      filter(a.fullname %in% input$selected_artist) %>% 
      filter(e.country %in% input$selected_country) %>% 
      filter(e.venue %in% input$selected_venue) %>% 
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
  
  
  # TODO: this code lets you get the width of the plot vals_over_time
  # right_col_width <- reactive(session$clientData[["output_vals_over_time_width"]])
    

}

# Run the application 
shinyApp(ui = ui, server = server)
