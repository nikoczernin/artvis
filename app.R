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
        height:25vh;
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
    # i wrap each MC widget in a div to be able to specify their height
    column(2,
           htmlOutput("variable_selection"),  # Dropdown for selecting the variable to count
           # Multi-select for selecting artists
           div(
             htmlOutput("artist_selection"),
             class = "scrollBox",
            ),
           div(
             htmlOutput("country_selection"),   # Multi-select for selecting countries
             class = "scrollBox",
            ),
           div(
             htmlOutput("venue_selection"),      # Multi-select for selecting venues
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
             leafletOutput("choropleth"),    # Interactive choropleth map
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
      a.fullname = a.fullname.rev,
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
      choices = list("Number of exhibitions"="e.id", 
                     "Number of paintings"="paintings"),
    )
  })
  
  
  
  ##### Grouped Variable Aggregation ##### 
  # get the aggregate number per group of the selected variable at interest
  get_aggregate <- function(data, grouping.var, keep.groups=TRUE){
    # aggregate the selected variable per artist
    aggregated.data <- data %>% 
      # apply the date filter
      filter(e.startdate %>% between(input$period[1], input$period[2])) %>% 
      group_by(across(all_of(grouping.var))) %>% 
      # depending on the selected grouping.variable
      # perform a different aggregation
      # for "Number of exhibitions": count unique exhibition IDs per group
      # for "Number of paintings": sum up painting counts of all exhibition per group
      {
        if (input$var_of_interest == "e.id") summarise(., n = n_distinct(e.id))
        else summarise(., n = sum(e.paintings, na.rm = TRUE))
      } %>% 
      # now join it back with all artists so that filtered out artists
      # get a zero
      right_join(data %>% select(!!grouping.var) %>% unique(), by=c(grouping.var)) %>%
      mutate(n = ifelse(is.na(n), 0, n)) %>%
      arrange(!!sym(grouping.var))
    # if groups are not asked for in keep.groups, only return the values
    if (!keep.groups) aggregated.data <- aggregated.data$n
    aggregated.data
  }
  
  
  
  ###### Get aggregates of selected variable #####
  #### Get artist aggregates
  artist.totals <- reactive({
    req(input$var_of_interest)
    req(input$period)
    # aggregate the selected variable per artist
    artvis %>% get_aggregate("a.fullname", keep.groups=FALSE)
  })
  
  
  #### Get country aggregates
  country.totals <- reactive({
    req(input$var_of_interest)
    req(input$period)
    # aggregate the selected variable per artist
    artvis %>% get_aggregate("e.country", keep.groups=FALSE)
  })
  
  
  #### Get venue aggregates
  venue.totals <- reactive({
    req(input$var_of_interest)
    req(input$period)
    # aggregate the selected variable per artist
    artvis %>% get_aggregate("e.venue", keep.groups=FALSE)
  })
  
  
  
  
  
  ###### Get unique category values #####
  
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
      full_join(data.frame(year=all_years), by="year") %>% 
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
    req(input$artist_selection)
    req(input$country_selection)
    req(input$venue_selection)
    req(input$period)
    
    # prepare the plot data (summarise)
    artvis %>%
      # apply the filters
      filter(a.fullname %in% input$artist_selection) %>% 
      filter(e.country %in% input$country_selection) %>% 
      filter(e.venue %in% input$venue_selection) %>% 
      get_aggregate("e.country") %>%
      # pass it to the choropleth
      choropleth("n", "e.country")
  })
  
  
  
  
  ##### Barplot function for top k value groups #####
  top_k_barplot <- function(grouping.var, k=10){
    artvis %>% 
      get_aggregate(grouping.var) %>% 
      arrange(desc(n)) %>% 
      head(k) %>% 
      ggplot(aes(x=fct_reorder(!!sym(grouping.var), n, .desc = T), y=n, fill=n)) +
      geom_bar(stat = "identity") +
      theme_minimal() +
      theme(
        axis.title = element_blank(),
        # axis.text = element_blank(),
        # axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1),
        # plot.margin = margin(t = 5, r = 5, b = 20, l = 5),
        panel.grid = element_blank()
      )
  }
  
  ##### Top Artists Barplot #####
  output$top_k_artists <- renderPlot({
    req(input$artist_selection)
    req(input$country_selection)
    req(input$venue_selection)
    req(input$period)
    top_k_barplot("a.fullname")
    })
  
  ##### Barplot Top Countries #####
  output$top_k_countries <- renderPlot({
    req(input$artist_selection)
    req(input$country_selection)
    req(input$venue_selection)
    req(input$period)
    top_k_barplot("e.country")
    })
  
  ##### Barplot Top Venues #####
  output$top_k_venues <- renderPlot({
    req(input$artist_selection)
    req(input$country_selection)
    req(input$venue_selection)
    req(input$period)
    top_k_barplot("e.venue")
    })
  
  
  # TODO: this code lets you get the width of the plot vals_over_time
  # right_col_width <- reactive(session$clientData[["output_vals_over_time_width"]])
    

}

# Run the application 
shinyApp(ui = ui, server = server)
