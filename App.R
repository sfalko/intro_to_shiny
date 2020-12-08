library(shiny)
library(tidyverse)
covid19 <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv")
library(rsconnect)





ui <- fluidPage(
  h1("COVID-19 Cases Over Time"),
  selectInput("states",
              "select the states you would like to compare:",
              multiple = TRUE,
              choices = covid19$state),
  submitButton(text = "Compare!"),
  plotOutput(outputId = "timeplot")
)

server <- function(input, output) {
  output$timeplot <- renderPlot({
    covid19 %>% 
      filter(state %in% input$states,
             cases >= 20) %>% 
      mutate(days_after_20 = (date - min(date))) %>% 
      ggplot() +
      geom_line(aes(x = days_after_20, y = cases, color = state)) +
      labs(x = "Days After 20+ Cases",
           y = "cases",
           color = "state") +
      scale_y_log10(label = scales::label_comma()) +
      theme_minimal()
  })
}

shinyApp(ui = ui, server = server)