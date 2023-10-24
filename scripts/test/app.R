library(shiny)
library(dplyr)
library(ggplot2)
library(googlesheets4)
library(rsconnect)
library(DT)
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

reviewer_list <- c("LP",
                   "SB",
                   "BW",
                   "KM",
                   "MW")

species_list <- c("Grizzly Bear",
                  "Boar",
                  "Beaver",
                  "Buffalo",
                  "Mountain Lion",
                  "Wolf",
                  "Other")

conflict_type <- c("Human-Wildlife",
                   "Human-Human",
                   "Nature-Wildlife",
                   "Unstated Conflict")

conflict_focus <- c("Wildlife",
                    "People",
                    "Policy",
                    "Practicioner",
                    "Ecosystem")

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
    dataTableOutput("entries", width = 300), tags$hr(),
    titlePanel("Wildlife Conflict Data Entry"),
    textInput("article_title", "Article Title", ""),
    textInput("publication", "Publication", ""),
    selectInput("state", "State" , state.abb, selected = ""),
    textInput("city", "City", ""),
    selectInput("species", "Species", choices = species_list, selected = ""),
    selectInput("reviewer", "Reviewer", choices = reviewer_list, selected = ""),
    dateInput("review_date", "Review Date", "2023-09-01", format = "mm/dd/yyyy"),
    selectInput("type", "Type", choices = conflict_type, selected = ""),
    selectInput("focus", "Focus", choices = conflict_focus, selected = ""),
    sliderInput("value_orientation", "Value Orientation", min = 1, max = 7, value = 1),
    textInput("comments", "Comments", ""),
    actionButton("submit", "Submit"),
    ),
  
  # Define server logic ----
  server <- function(input, output, session) {
    
    # Whenever a field is filled, aggregate all form data
    formData <- reactive({
      as.character(input$review_date) 
      # code above results in Warning: Error in $<-: Can't modify read-only reactive value 'review_date'
      data <- sapply(fields, function(x) input[[x]])
      data
      #data$review_date <- format(as.Date(date$review_date,  origin="2023-01-01"), "%m/%d/%Y")
    })
    
    
    # When the Submit button is clicked, save the form data
    observeEvent(input$submit, {
      #data.frame(review_date = as.character(format(input$review_date, "yyyy-mm-dd")))
      #data.frame(review_date = as_date(input$review_date))
      saveData(formData())
    })
    
    # Show the previous entries. Can take this out
    # (update with current entry when Submit is clicked)
    output$entries <- renderDataTable({
      input$submit
      #data$review_date <- format(as.Date(date$review_date,  origin="2023-01-01"), "%m/%d/%Y")
      loadData()
    })     
  }
)
#data$date <- format(as.Date(data$date, origin="1970-01-01"), "%m/%d/%Y")

# Run the app ----
shinyApp(ui = ui, server = server)


