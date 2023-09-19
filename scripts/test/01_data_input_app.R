library(shiny)
library(dplyr)
library(ggplot2)


# Define UI for app that can generate a csv file from input options
ui <- fluidPage(
  titlePanel("Wildlife Conflict Data Entry"),
  
  fluidRow(
    column(3,
           textInput("article_title",
           h3("Article Title"),
           value = "")),
    column(3,
           textInput("publication",
           h3("Publication"),
           value = "")),
    column(3,
           selectInput("state",
           h3("State"),
           choices = list("AL" = 1,
                          "AK" = 2,
                          "AZ" = 3,
                          "CA" = 4),
           selected = 1)),
   column(3,
          textInput("city",
                    h3("City"),
                    value = "")),
   column(3,
          selectInput("animal", 
                      h3("Animal"),
                      choices = list("Grizzly Bear" = 1,
                                     "Boar" = 2,
                                     "Beaver" = 3,
                                     "Buffalo" = 4,
                                     "Mountain Lion" = 5),
                      selected = 1)),
    column(3,
           dateInput("reviewer_1_date",
                     h3("Reviewer 1 Date"),
                     value = "2023-01-01")),
    column(3,
           selectInput("reviewer_1",
                       h3("Reviewer 1"),
                       choices = list("LP" = 1,
                                      "SB" = 2,
                                      "BW" = 3,
                                      "KM" = 4,
                                      "MW" = 5),
                       selected = 1)),
    column(3,
           selectInput("type", 
                       h3("Type"),
                       choices = list("Human-Wildlife" = 1,
                                      "Human-Human" = 2,
                                      "Nature-Wildlife" = 3),
                       selected = 1)),
    column(3,
           selectInput("problem", 
                       h3("Problem"),
                       choices = list("Wildlife" = 1,
                                      "People" = 2,
                                      "Policy" = 3,
                                      "Practicioner" = 4),
                       selected = 1)),
    column(3,
           sliderInput("value",
                       h3("Value Orientation"),
                       min = 1, max = 5,
                       value = 1)),
   column(3,
          textInput("comments",
                    h3("Comments"),
                    value = ""))
  )
)

# Define server logic ----
server <- function(input, output) {
  
}

# Run the app ----
shinyApp(ui = ui, server = server)


