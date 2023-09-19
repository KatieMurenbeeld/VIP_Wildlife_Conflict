library(shiny)
library(dplyr)
library(ggplot2)

# Define UI for app that can generate a csv file from input options
ui <- fluidPage(
  titlePanel("Wildlife Conflict Data Entry"),
  
  fluidRow(
    column(3,
           dateInput("date",
                     h3("Date"),
                     value = "2023-01-01")),
    column(6,
           selectInput("code_1", 
                       h3("Code 1"),
                       choices = list("Grazing conflicts with wildlife" = 1,
                                      "Wildlife conflicts with recreation" = 2,
                                      "Wildlife conflicts with homeowner" = 3,
                                      "etc." = 4))),
    column(4,
           selectInput("animal", 
                       h3("Animal"),
                       choices = list("Grizzly Bear" = 1,
                                      "Boar" = 2,
                                      "Beaver" = 3,
                                      "Buffalo" = 4,
                                      "Mountain Lion" = 5)))
  )
)
