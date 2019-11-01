
########
# shiny quakes app
# A. Putt
# GEOL590  |  2019.10.31
########

library(shiny)
library(tidyverse)
library(ggplot2)

#quakes dataset is a list of earth quake site location lat and long, station location, depth, magnitude (from richter scale)
# We'll limit the range of selectable carats to teh actual range of earthquake magnitudes
min1.mag <- min(quakes$mag)
max1.mag <- max(quakes$mag)

# Need a vector of axis variables as characters
axis_vars <- names(quakes)

# Create a character vector of those columns of of the quakes dataset are all viewed as logical
factor.indices <- vapply(quakes, is.factor, TRUE) 
factor.columns <- axis_vars[factor.indices]


# Define UI for application that greates the page users interact with
ui <- fluidPage(
  
  # add title to app
  titlePanel("Earth Quake Grapher"),
  
  # Sidebar with sliders placed into the side panel
  sidebarLayout(
    sidebarPanel(
      
      # This is a range slider (i.e. there's a max and min). It is set that way by "value" (the starting value), which is a 2-element vector
      sliderInput("magrange",
                  "Earth Quake Magnitude",
                  min = min1.mag,
                  max = max1.mag,
                  value = c(min1.mag, max1.mag)),
      
      
      # Select x and y variables
      selectInput(inputId = "xvar",
                  label = "X axis",
                  choices = axis_vars,
                  selected = "x"),
      
      selectInput(inputId = "yvar",
                  label = "Y axis",
                  choices = axis_vars,
                  selected = "y"),
      #will not run until you hit the go button. this displays the word 
      actionButton("go", 
                   "Run",
                   icon = icon("globe-asia"))
    ),
    
    # Show a plot of diamonds data frame. This output doesn't care what that plot is, only that it will be associated with output$diamonds_plot
    mainPanel(
      plotOutput("quake_plot")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # Filter diamonds based on carat - this doesn't strictly need to be in reactive,
  # ...but maybe we want to expand the app later, in which case it'll be useful to have it in reactive()
  filt_mag <- reactive({
    quakes %>%
      filter(mag >= min(input$magrange)) %>%
      filter(mag <= max(input$magrange))
  })
  
  # Make the plot
  # eventReactive listens for a change in state of input$go, and only runs the code when the state changes
  # Note use of aes_string, since input$xvar returns a string, not a reference to the object ("carat" not carat)
  p_quake <- eventReactive(input$go, {
    ggplot(filt_mag(), aes_string(x = input$xvar, y = input$yvar, colour = "depth")) + # Note that you need () after filt_dia, since filt_dia() is a function to get the object you want, not the actual object
      geom_point()
  })
  
  
  # Create diagnostic output window to show what kind of output the double slider creates
  output$diagnostic <- renderText(
    input$magrange
  )
  
  # Create a dynamic plot plot
  # I moved the ggplot into its own reactive context.
  # Note that creating a ggplot object is very fast - it is actually drawing the plot that is slow.
  output$quake_plot <- renderPlot(
    p_quake()
  )
  
}

# Run the application 
shinyApp(ui = ui, server = server)
