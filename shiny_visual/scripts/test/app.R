## Visual App


library(shiny)
library(dplyr)
library(ggplot2)
library(googlesheets4)
library(googledrive)
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

#gs4_auth(cache = ".secrets", email = "katiemurenbeeld@boisestate.edu")

#folder_url <- "https://drive.google.com/drive/u/0/folders/1ob5sagTtT3svhc7ZKeemd9TiAq1_MsCL"
#folder <- drive_get(as_id(folder_url))

#gdrive_files <- drive_ls(folder)
#id <- gdrive_files[gdrive_files$name == "New Article Coding Framework", ]$id
#drive_download(id, path = "shiny_visual/data/original/new_codes.csv", overwrite = TRUE)

article_codes <- read.csv(file = 'shiny_visual/data/original/new_codes.csv')

species_list <- c(unique(article_codes$Species))

#sheet_id <- "https://docs.google.com/spreadsheets/d/1EFbr-GahLJ0Hl01YYheOAjRny8GFxHryFjnD5RNbQUY/edit#gid=0"

#data <- read_sheet(sheet_id)

library(shiny)

ui <- fluidPage(
  fluidRow(
    column(6,
           selectInput("species", "Species", choices = species_list)
    )
  ),
  fluidRow(
    column(4, tableOutput("Publication.State")),
    column(4, tableOutput("Type.of.Conflict")),
    column(4, tableOutput("Focus.is")),
    column(4, tableOutput("Value.Orientation.1.7"))
  ),
  fluidRow(
    column(12, plotOutput("state_conflict"))
  )
)

server <- function(input, output, session) {
  selected <- reactive(article_codes %>% filter(Species == input$species))
  
  output$Publication.State <- renderTable(
    selected() %>% count(Publication.State, wt = weight, sort = TRUE)
  )
  output$Type.of.Conflict <- renderTable(
    selected() %>% count(Type.of.Conflict, wt = weight, sort = TRUE)
  )
  output$Focus.is <- renderTable(
    selected() %>% count(Focus.is, wt = weight, sort = TRUE)
  )
  output$Value.Orientation.1.7 <- renderTable(
    selected() %>% count(Value.Orientation.1.7, wt = weight, sort = TRUE)
  )
  
#  summary <- reactive({
#    selected() %>%
#      count(Publication.State, Type.of.Conflict) %>%
#      left_join(population, by = c("age", "sex")) %>%
#      mutate(rate = n / population * 1e4)
#  })
  
#  output$state_conflict <- renderPlot({
#    summary() %>%
#      ggplot(aes(age, n, colour = sex)) +
#      geom_line() +
#      labs(y = "Estimated number of injuries")
#  }, res = 96)
}

shinyApp(ui, server)