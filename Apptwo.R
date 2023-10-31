################################################################
# Shiny app by Grant Gunderson
# Oct 30, 2023
#
# Allows for the user to check traffic accident data based on
# state and weather type.
#
# 
# Source code at GitHub: https://......
################################################################

library(shiny)
library(readxl)
library(dplyr)
library(ggplot2)
accident <- read.csv("accident.csv")
weather <- read.csv("weather.csv")
accident_combined <- merge(accident, weather, by = "ST_CASE")
na.omit(accident_combined)

accident_final <- accident_combined %>%
  select(ST_CASE, STATENAME.x, COUNTYNAME, CITYNAME, MONTHNAME, DAYNAME,
         DAY_WEEKNAME, YEAR, HOUR, ROUTENAME, RUR_URBNAME, WEATHERNAME.x)
weather <- unique(accident_final$WEATHERNAME.x)

ui <- fluidPage(
  
  # Application title
  titlePanel("Traffic Crash Data Per State Based on Different Weather Types"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      selectInput("state_input",
                  "Select State:",
                  choices = unique(accident_final$STATENAME.x)),
      selectInput("select", "Select", 
                  choices = c("DAY_WEEKNAME",
                              "MONTHNAME",
                              "ROUTENAME")
      ),
      checkboxGroupInput("RuralUrban", "Select", 
                         choices = c("Urban", "Rural")
      ),
      checkboxGroupInput("select2", "Select", 
                         choices = weather
      )
    ),
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$distPlot <- renderPlot({
    # generate bins based on input$bins from ui.R
    
    state    <- accident_final %>%
      filter(STATENAME.x == input$state_input)
    if (is.null(input$select2)) {
      weatherchoice   <- state
    } else {
      weatherchoice   <- state %>%
        filter(WEATHERNAME.x %in% input$select2)
    }
    if (is.null(input$RuralUrban)) {
      Rural   <- weatherchoice
    } else {
      Rural   <- weatherchoice %>%
        filter(RUR_URBNAME %in% input$RuralUrban)
    }
    final     <- Rural[, input$select]
    
    counts <- as.data.frame(table(final))
    colnames(counts)[colnames(counts) == 'final'] <- input$select
    p <- ggplot(data = counts) +
      aes(x = counts[, input$select], y = Freq, fill = counts[, input$select], label = Freq) +
      geom_bar(stat = "identity") +
      xlab(input$select)+
      theme(axis.text.x=element_text(size=18)) +
      theme(axis.text.y=element_text(size=18)) +
      theme(legend.text = element_text(size = 18),
            legend.title = element_text(size = 18)) +
      theme(axis.title.x = element_text(size = 18),
            axis.title.y = element_text(size = 18)) +
      geom_text(aes(label=Freq), vjust=0, size = 7)
    p <- p + guides(fill=guide_legend(title= input$select))
    p <- p + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
    print(p)
  }, height = 800, width = 1000)
}

# Run the application 
shinyApp(ui = ui, server = server)