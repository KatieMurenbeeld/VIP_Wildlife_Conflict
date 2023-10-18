library(shiny)
library(dplyr)
library(ggplot2)
library(googlesheets4)
library(rsconnect)
library(lubridate)


### Before running the app copy the following lines of code into the console
# library(googlesheets4)
# setwd('/Users/kathrynmurenbeeld/Analysis/VIP_Wildlife_Conflict/scripts/test/')
# gs4_auth(email = "your@email.edu", cache = ".secrets")
# Make sure to update your .gitignore to include .secrets and */.secrets
# You will be taken to an authorization page, make sure to check the box that allows for editing
###

gs4_auth(cache = ".secrets", email = "katiemurenbeeld@boisestate.edu")

sheet_id <- "https://docs.google.com/spreadsheets/d/1EFbr-GahLJ0Hl01YYheOAjRny8GFxHryFjnD5RNbQUY/edit#gid=0"

# the fields need to match the google sheet column headers AND the input IDs
fields <- c("article_title",	"publication",	"state",
            "city",	"species",	"reviewer",	"review_date",
            "type", "focus",	"value_orientation",	"comments")

# Define function to use in server logic
table <- "entries"

saveData <- function(data) {
  # The data must be a dataframe rather than a named vector
  data <- data %>% as.list() %>% data.frame()
  # Add the data as a new row
  sheet_append(sheet_id, data)
}

loadData <- function() {
  # Read the data
  read_sheet(sheet_id)
}


# Define UI for app that can append to a google sheet  from input options
shinyApp(
  ui <- fluidPage(
    DT::dataTableOutput("entries", width = 300), tags$hr(),
    titlePanel("Wildlife Conflict Data Entry"),
    textInput("article_title", "Article Title", ""),
    textInput("publication", "Publication", ""),
    selectInput("state", "State" ,
               choices = list("AL", "AK", "AZ", "AR", "CA",
                            "CO", "CT", "DE", "DC", "FL",
                            "GA", "HI", "ID", "IL", "IN",
                            "IA", "KS", "LA", "ME", "MA",
                            "MD", "MA", "MI", "MN", "MS",
                            "MO", "MT", "NE", "NV", "NH",
                            "NJ", "NM", "NY", "NC", "ND",
                            "OH", "OK", "OR", "PA", "RI",
                            "SC", "SD", "TN", "TX", "UT",
                            "VT", "VA", "WA", "WV", "WI",
                            "WY"),
             selected = ""),
    textInput("city", "City", ""),
    selectInput("species", "Species",
                choices = list("Grizzly Bear",
                               "Boar",
                               "Beaver",
                               "Buffalo",
                               "Mountain Lion",
                               "Wolf",
                               "Other"),
                selected = ""),
    selectInput("reviewer", "Reviewer",
                choices = list("LP",
                               "SB",
                               "BW",
                               "KM",
                               "MW"),
                selected = ""),
    dateInput("review_date", "Review Date", "2023-09-01", format = "yyyy/mm/dd"),
    selectInput("type", "Type",
                choices = list("Human-Wildlife",
                               "Human-Human",
                               "Nature-Wildlife",
                               "Unstated Conflict"),
                selected = ""),
    selectInput("focus", "Focus",
                choices = list("Wildlife",
                               "People",
                               "Policy",
                               "Practicioner",
                               "Ecosystem"),
                selected = ""),
    sliderInput("value_orientation", "Value Orientation",
                min = 1, max = 7,
                value = 1),
    textInput("comments", "Comments", ""),
    actionButton("submit", "Submit")
    ),
  
  # Define server logic ----
  server <- function(input, output, session) {
    
    # Whenever a field is filled, aggregate all form data
    formData <- reactive({
      data <- sapply(fields, function(x) input[[x]])
      data
    })
    
    # When the Submit button is clicked, save the form data
    observeEvent(input$submit, {
      data.frame(A = as.character(format(input$review_date, "yyyy-mm-dd")))
      saveData(formData())
    })
    
    # Show the previous entries
    # (update with current entry when Submit is clicked)
    output$entries <- DT::renderDataTable({
      input$submit
      loadData()
    })     
  }
)


# Run the app ----
shinyApp(ui = ui, server = server)


