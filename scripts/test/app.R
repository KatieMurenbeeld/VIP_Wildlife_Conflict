library(shiny)
library(shinyFeedback)
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
fields <- c("Article Title", "Old Spreadsheet",	"Article Type", 
            "Newspaper", "Publication City", "Publication State",
            "Link", "Species", "Reviewer1", "Reviewer1_date", 
            "Reviewer2", "Reviewer2_date", "Type of Conflict",
            "Focus is", "Value Orientation(1-7)", "Notes")

articletype_list <- c("newswire", 
                      "online", 
                      "blog", 
                      "newspaper", 
                      "local article")

reviewer_list <- c("LPotter",
                   "SBreedlove",
                   "BWall",
                   "KMurenbeeld",
                   "MWilliamson",
                   "MG", 
                   "PGill",
                   "HK",
                   "Not reviewed")

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
  read_sheet(sheet_id, col_types = "cicccccccDcDccic")
}


# Define UI for app that can append to a google sheet  from input options
shinyApp(
  ui <- fluidPage(
    #useShinyFeedback(),
    dataTableOutput("entries", width = 300), tags$hr(),
    titlePanel("Wildlife Conflict Data Entry"),
    textInput("Article Title", "Article Title", ""),
    numericInput("Old Spreadsheet", "Old Spreadsheet", value = NA),
    selectInput("Article Type", "Article Type", 
                choices = articletype_list, 
                selected = ""),
    textInput("Newspaper", "Newspaper", value = ""),
    textInput("Publication City", "Publication City", ""),
    selectInput("Publication State", "Publication State" , 
                state.abb, 
                selected = ""),
    textInput("Link", "Link", "https://"),
    selectInput("Species", "Species", 
                choices = species_list, 
                selected = ""),
    selectInput("Reviewer1", "Reviewer1", 
                choices = reviewer_list, 
                selected = ""),
    dateInput("Reviewer1_date", "Reviewer1 Date", 
              value = Sys.Date(),
              format = "mm/dd/yyyy"),
    selectInput("Reviewer2", "Reviewer2", 
                choices = reviewer_list, 
                selected = ""),
    dateInput("Reviewer2_date", "Reviewer2 Date", 
              value = NA,
              format = "mm/dd/yyyy"),
    #textInput("review_date", "Review Date", "mm/dd/yyyy"),
    #textOutput("date"),
    selectInput("Type of Conflict", "Type of Conflict", 
                choices = conflict_type, 
                selected = ""),
    selectInput("Focus is", "Focus is", 
                choices = conflict_focus, 
                selected = ""),
    sliderInput("Value Orientation(1-7)", "Value Orientation", 
                min = 1, max = 7, value = 1),
    textInput("Notes", "Notes", ""),
    actionButton("submit", "Submit"),
    ),
  
  # Define server logic ----
  server <- function(input, output, session) {
    
    
    # Whenever a field is filled, aggregate all form data
    formData <- reactive({
      data <- sapply(fields, function(x) input[[x]])
      #data <- data %>% as.list() %>% 
      #  data.frame(as.character(input$review_date))
    })
    
    
    # When the Submit button is clicked, save the form data
    #observeEvent(input$submit, {
    #  saveData(formData())
    #})
    
    # Show the previous entries. Can take this out
    # (update with current entry when Submit is clicked)
    output$entries <- renderDataTable({
      input$submit
      loadData()
    })     
  }
)

# Run the app ----
shinyApp(ui = ui, server = server)


