#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
artvis
library(shiny)
library(stringr)
library(tidyverse)


# Define UI for application that draws a histogram
ui <- fluidPage(
    
  fluidRow(
    column(2,
      htmlOutput("var_selection"),
           ),
    column(10,
      plotOutput("vals_over_time"),
      htmlOutput("time_selection"),
      textOutput("display_period"),
           ),
    # width="100%"
  ),
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  # read in the NEW preprocessed data
  artvis <- read.csv("artvis_dump_NEW.csv", sep = ";") %>% print()
  
  # when grouping data over the different time periods, you need to 
  # get a set of all years in order
  all_years <- as.numeric(min(artvis$e.startdate)):as.numeric(max(artvis$e.startdate))
    
  output$var_selection <- renderUI({
    selectizeInput(
      "var_of_interest",
      "Select a variable to count",
      choices = list("Number of exhibitions"="exhibitions", 
                     "Number of paintings"="paintings")
    )
  })
    

    output$time_selection <- renderUI({
      min_val = all_years %>% min()
      max_val = all_years %>% max()

      sliderInput("period",
                  "",
                  sep = "",
                  width="100%",
                  min =   min_val,
                  max =   max_val,
                  value = c(min_val, max_val))
    })
    output$display_period <- renderText(input$period) 
    
    
    
    output$vals_over_time <- renderPlot({
      
      plotdata <- artvis %>% 
        group_by(e.startdate) %>% 
        summarise(n_exhibs = n_distinct(e.title)) %>% 
        mutate(year = e.startdate %>% as.numeric()) %>% 
        full_join(data.frame(year=all_years)) %>% 
        arrange(year) %>% 
        mutate(n_exhibs=ifelse(is.na(n_exhibs), 0, n_exhibs))
      
      plotdata %>% 
        ggplot(aes(x=year, y=n_exhibs)) +
          geom_bar(stat = "identity") +
          theme_minimal() +
          theme(
            axis.title = element_blank(),
            axis.text = element_blank(),
            panel.grid = element_blank()
          )
    })
    

    # output$distPlot <- renderPlot({
    #     # generate bins based on input$bins from ui.R
    #     x    <- faithful[, 2]
    #     bins <- seq(min(x), max(x), length.out = input$bins + 1)
    # 
    #     # draw the histogram with the specified number of bins
    #     hist(x, breaks = bins, col = 'darkgray', border = 'white',
    #          xlab = 'Waiting time to next eruption (in mins)',
    #          main = 'Histogram of waiting times')
    # })
}

# Run the application 
shinyApp(ui = ui, server = server)
