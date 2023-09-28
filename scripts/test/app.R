library(shiny)
library(dplyr)
library(ggplot2)
library(googlesheets4)

gs4_auth(cache = ".secrets", email = "katiemurenbeeld@boisestate.edu")

sheet_id <- "https://docs.google.com/spreadsheets/d/1EFbr-GahLJ0Hl01YYheOAjRny8GFxHryFjnD5RNbQUY/edit#gid=0"


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
           choices = list("AL" = 1, "AK" = 2, "AZ" = 3, "AR" = 4, "CA" = 5,
                          "CO" = 6, "CT" = 7, "DE" = 8, "DC" = 9, "FL" = 10,
                          "GA" = 11, "HI" = 12, "ID" = 13, "IL" = 14, "IN" = 15,
                          "IA" = 16, "KS" = 17, "LA" = 18, "ME" = 19, "MA" = 20,
                          "MD" = 21, "MA" = 22, "MI" = 23, "MN" = 24, "MS" = 25,
                          "MO" = 26, "MT" = 27, "NE" = 28, "NV" = 29, "NH" = 30,
                          "NJ" = 31, "NM" = 32, "NY" = 33, "NC" = 34, "ND" = 35,
                          "OH" = 36, "OK" = 37, "OR" = 38, "PA" = 39, "RI" = 40,
                          "SC" = 41, "SD" = 42, "TN" = 43, "TX" = 44, "UT" = 45,
                          "VT" = 46, "VA" = 47, "WA" = 48, "WV" = 49, "WI" = 50,
                          "WY" = 51),
           selected = 1)),
   column(3,
          textInput("city",
                    h3("City"),
                    value = "")),
   column(3,
          selectInput("species", 
                      h3("Species"),
                      choices = list("Grizzly Bear" = 1,
                                     "Boar" = 2,
                                     "Beaver" = 3,
                                     "Buffalo" = 4,
                                     "Mountain Lion" = 5,
                                     "Wolf" = 6,
                                     "Other" = 7),
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
                                      "Nature-Wildlife" = 3,
                                      "Unstated Conflict" = 4),
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
                       min = 1, max = 7,
                       value = 1)),
   column(3,
          textInput("comments",
                    h3("Comments"),
                    value = "")),
   
   actionButton("append", "Save Entry")
  )
)

# Need a button widget to append to google sheet

# Define server logic ----
server <- function(input, output) {
  }


# Run the app ----
shinyApp(ui = ui, server = server)


