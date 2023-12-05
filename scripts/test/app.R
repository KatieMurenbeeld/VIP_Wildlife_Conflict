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

#sheet_id <- "https://docs.google.com/spreadsheets/d/1EFbr-GahLJ0Hl01YYheOAjRny8GFxHryFjnD5RNbQUY/edit#gid=0"
sheet_id <- "https://docs.google.com/spreadsheets/d/18HV8cVgl0rRCB0_NHj-MjModPCWQ30prTOu2_jOr-Ik/edit#gid=0"

# the fields need to match the google sheet column headers AND the input IDs
fields <- c("Article_Title", "Old_Spreadsheet",	"Article_Type", 
            "Newspaper", "Publication_City", "Publication_State",
            "Link", "Species", "Reviewer1", "Reviewer1_date", 
            "Reviewer2", "Reviewer2_date", "Conflict_Type",
            "Focus", "Value_Orientation", "Notes")

articletype_list <- c("newswire", 
                      "online", 
                      "blog", 
                      "newspaper", 
                      "local article")

reviewer_list <- c("Not reviewed", 
                   "LPotter",
                   "SBreedlove",
                   "BWall",
                   "KMurenbeeld",
                   "MWilliamson",
                   "MGiles", 
                   "PGillis",
                   "HKruzich")

review_date_list <- c("NA",
                      "Fall 2022",
                      "Spring 2023",
                      "Summer 2023",
                      "Fall 2023",
                      "Spring 2024",
                      "Summer 2024",
                      "Fall 2024",
                      "Spring 2025",
                      "Summer 2025",
                      "Fall 2025",
                      "Spring 2026",
                      "Summer 2026",
                      "Fall 2026",
                      "Spring 2027")

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
  #data <- data %>% as.list() %>% data.frame() 
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
    textInput("Article_Title", "Article Title", ""),
    numericInput("Old_Spreadsheet", "Old Spreadsheet Number", value = NA),
    selectInput("Article_Type", "Article Type", 
                choices = articletype_list, 
                selected = ""),
    textInput("Newspaper", "Newspaper", value = ""),
    textInput("Publication_City", "Publication City", ""),
    selectInput("Publication_State", "Publication State", 
                state.abb, 
                selected = ""),
    textInput("Link", "Link", ""),
    selectInput("Species", "Species", 
                choices = species_list, 
                selected = ""),
    selectInput("Reviewer1", "Reviewer1", 
                choices = reviewer_list, 
                selected = ""),
    selectInput("Reviewer1_date", "Reviewer1 Date",
                choices = review_date_list,
                selected = "Fall 2023"),
    selectInput("Reviewer2", "Reviewer2", 
                choices = reviewer_list, 
                selected = ""),
    selectInput("Reviewer2_date", "Reviewer2 Date",
                choices = review_date_list,
                selected = "NA"),
    selectInput("Conflict_Type", "Type of Conflict", 
                choices = conflict_type, 
                selected = ""),
    selectInput("Focus", "Focus is", 
                choices = conflict_focus, 
                selected = ""),
    sliderInput("Value_Orientation", "Value Orientation", 
                min = 1, max = 7, value = 1),
    textInput("Notes", "Notes", ""),
    actionButton("submit", "Submit"),
    ),
  
  # Define server logic ----
  server <- function(input, output, session) {
    
    # Whenever a field is filled, aggregate all form data
    formData <- eventReactive(input$submit, {
      data <- sapply(fields, function(x) input[[x]])
      data <- data %>% as.list() %>% data.frame() 
      data$Old_Spreadsheet <- as.numeric(data$Old_Spreadsheet)
      data$Value_Orientation <- as.numeric(data$Value_Orientation)
      data
    })
    
    # When the Submit button is clicked, save the form data
    observeEvent(input$submit, {
      saveData(formData())
    })
    
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


