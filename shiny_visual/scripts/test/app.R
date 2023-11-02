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

gs4_auth(cache = ".secrets", email = "katiemurenbeeld@boisestate.edu")

folder_url <- "https://drive.google.com/drive/u/0/folders/1ob5sagTtT3svhc7ZKeemd9TiAq1_MsCL"
folder <- drive_get(as_id(folder_url))

gdrive_files <- drive_ls(folder)
id <- gdrive_files[gdrive_files$name == "New Article Coding Framework", ]$id
drive_download(id, path = "data/new_codes.csv", overwrite = TRUE)

article_codes <- read.csv(file = 'data/new_codes.csv')

#sheet_id <- "https://docs.google.com/spreadsheets/d/1EFbr-GahLJ0Hl01YYheOAjRny8GFxHryFjnD5RNbQUY/edit#gid=0"

#data <- read_sheet(sheet_id)

library(shiny)

ui <- fluidPage(
  
)

server <- function(input, output, session) {
  
}

shinyApp(ui, server)