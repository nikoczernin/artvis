# Author: [Nikolaus Czernin]
# Date: [15.1.2025]
# Description: A Shiny app to visualize art exhibition data. 
# Includes interactive choropleth maps, bar plots, and filter options.


# Required Libraries
library(shiny)         # Core shiny library for building the app
library(stringr)       # String manipulation functions
library(tidyverse)     # Data manipulation and visualization
library(shinyWidgets)  # Additional UI widgets for Shiny
library(leaflet)       # Interactive maps
library(RColorBrewer)  # Custom color maps
library(scales)        # adding linebreaks to x scale ticks
library(countrycode)
library(shinybrowser)

# Source external helper functions for choropleth map
source("./choropleth.R")


##### User Interface (UI) for the Shiny App #####
ui <- fluidPage(
  shinybrowser::detect(),
  tags$head(
    # Custom CSS for inline plots
    tags$style(HTML("
      
      /* BODY */
      body {
        background: #F8F8F8;
      }
      
      /* BOXES */
      .box {
        background: #FFF;
        border-radius: 20px;
        box-shadow: 2px 2px 40px #BFBFBF;
        margin: 20px;
        padding: 20px 30px;
      }
      
      /* LEFTMOST BOX SHOULD HAVE NO LEFT MARGIN, AND VICE VERSA FOR RIGHTMOST */
      .leftmost {
      }
      .rightmost {
      }
      
      /*
      .height20 {
        height: 20vh;
      }
      */
      
      .flex-container {
        display: flex;
        justify-content: space-between;
        padding: 0px 20px;
      }

      .flex-container > div {
        margin: 0px 20px;
      }
      
      /* 30% with inline barplots */
      .inline-plot {
        display: inline-block;
        width: 32%; 
        vertical-align: top;
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
      
      
      /* TIMELINE IS A LITTLE TOO WIDE */
      .time-selector {
        margin-left:  4%;
        margin-right: 3%;
      }
      
      .timebox {
        padding-bottom:0;
      }
      
      h2 {
        font-family: 'Georgia', 'Times New Roman', serif; 
        font-size: 18px; 
        font-weight: normal; 
        color: #333333; 
        letter-spacing: 2px; 
        margin-top: 0px; 
        margin-bottom: 15px;
        border-bottom: 2px solid #999999; /* Underline for decoration */
        padding-bottom: 5px; /* Space between the text and the underline */
      }

      
    "))
  ),
  
  
  # Layout with two columns
  fluidRow(
    textOutput("display"),   # Display for debugging
    # Left column for filter selection
    # i wrap each MC widget in a div to be able to specify their height
    column(3,
           # Dropdown for selecting the variable to count
           div(
             tags$h2( "Select a variable"),
             htmlOutput("variable_selection"), 
             class = "box",
            ),
           # Multi-select for selecting artists
           div(
             tags$h2( "Artist selection"),
             htmlOutput("artist_selection"),
             class = "scrollBox box",
            ),
           # Multi-select for selecting countries
           div(
             tags$h2( "Country selection"),
             htmlOutput("country_selection"),   
             class = "scrollBox box",
            ),
           # Multi-select for selecting venues
           div(
             tags$h2( "Venue selection"),
             htmlOutput("venue_selection"),      
             class = "scrollBox box",
            ),
    ),
    # Right column for visualizations
    column(9,
           fluidRow(
             plotOutput("vals_over_time", 
                        height = "100px"
                        ), # Bar plot for values over time
             # Slider input for selecting time range
             div(htmlOutput("time_selection"), class="time-selector"),
             textOutput("test"),
             class="box timebox"
           ),
           # Interactive choropleth map
           fluidRow(
             leafletOutput("choropleth"),    
             # height="60%", 
             class="box "),
           # Three inline bar plots for top artists, countries, and venues
           fluidRow(div(class = "flex-container",
              div(class = "inline-plot box leftmost", 
                tags$h2( "Top 10 Artists"),
                plotOutput("top_k_artists")),
              div(class = "inline-plot box", 
                tags$h2("Top 10 Countries"),
                plotOutput("top_k_countries")),
              div(class = "inline-plot box rightmost", 
                tags$h2("Top 10 Venues"),
                plotOutput("top_k_venues")),
             # height = "30%"
           )),
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
    ) %>% 
    mutate(e.country.name = e.country %>% countrycode("iso2c", "country.name.en"))
    # print()
  
  # Define colors for active and inactive data points
  color.active <- "orange"
  color.inactive <- "gray"
  color.scale <- "BuGn"
  
  
  # Extract the range of years from the dataset
  first_year <- artvis$e.startdate %>% na.omit() %>% min() %>% as.numeric()  
  last_year  <- artvis$e.startdate %>% na.omit() %>% max() %>% as.numeric()  
  all_years <- seq(first_year, last_year)
    
  
  
  ##### Variable Selection #####
  # Dropdown for choosing a variable to count
  output$variable_selection <- renderUI({
    selectizeInput(
      inputId = "var_of_interest",
      # label = "Select a variable to count",
      label = "",
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
    artvis %>% get_aggregate("e.country.name", keep.groups=FALSE)
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
    .$e.country.name %>% 
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
  multiple_selection_container <- function(intputId, label="", options, values=NULL, show.value=TRUE){
    # options: vector of option values
    # prefix: prefix of input variable name
    renderUI({
      # prepare the classes
      # if no values have been passed, just use empty values (not 0 though)
      if (is.null(values)) {
        values <- function() rep("", length(options))
        # add the total value of each option to the end of it in parenthesis
        # TODO: this is currently not working, but who cares
        if (show.values) options <- paste0(options, " (", values, ")")
      }
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
    # label = "Select artists",
    options = all.artists,
    values = artist.totals
  )
  
  ##### Country Selection Widget #####
  output$country_selection <- multiple_selection_container(
    intputId = "country_selection",
    # label = "Select countries",
    options = all.countries,
    values = country.totals
  )

  ##### Venue Selection Widget #####
  output$venue_selection <- multiple_selection_container(
    intputId = "venue_selection",
    # label = "Select venues",
    options = all.venues,
    values = venue.totals
  )
  
  
  
  
  
  
  
  
    

  ##### Timeline Slider #####
  # Slider input for selecting a time range
  output$time_selection <- renderUI({
    min_val = all_years %>% min()
    # add 1 extra year so that the timeline picker surrounds all years
    max_val = (all_years %>% max())+1

    sliderTextInput("period",
                NULL,
                width="100%",
                grid = TRUE,
                animate = TRUE,
                selected = c(min_val, max_val),
                choices=seq(min_val, max_val)
    )
  })
  
  # output$test <- renderText(shinybrowser::get_height())
  
  
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
      # filter(a.fullname %in% input$artist_selection) %>% 
      # filter(e.country %in% input$country_selection) %>% 
      # filter(e.venue %in% input$venue_selection) %>% 
      get_aggregate("e.country") %>%
      # pass it to the choropleth
      choropleth("n", "e.country", colors=color.scale)
  })
  
  
  ##### Barplot function for top k value groups #####
  top_k_barplot <- function(grouping.var, k=10){
    artvis %>% 
      get_aggregate(grouping.var) %>% 
      arrange(desc(n)) %>% 
      head(k) %>% 
      # add line breaks to the grouping variable, so that long names dont make the plot narrow
      # mutate(grouping.var = !!sym(grouping.var) %>% str_wrap(width=20)) %>%
      ggplot(aes(x=fct_reorder(!!sym(grouping.var), n), y=n, fill=n)) +
        geom_bar(stat = "identity") +
        theme_minimal() +
        scale_fill_distiller(palette = color.scale, direction = 1) +  
        coord_flip() +  # Make the bars horizontal
        guides(fill="none") +
        scale_x_discrete(labels = label_wrap(20)) +
        theme(
          axis.title = element_blank(),
          axis.text = element_text(size = 10),
          # plot.margin = margin(t = 5, r = 5, b = 20, l = 5),
          panel.background = element_rect(fill = "transparent", color = NA), 
          plot.background = element_rect(fill = "transparent", color = NA),  
          legend.background = element_rect(fill='transparent'),
          legend.box.background = element_rect(fill='transparent'),
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
  

}

# Run the application 
shinyApp(ui = ui, server = server)
