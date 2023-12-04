## Visual App


library(shiny)
library(dplyr)
library(ggplot2)
library(rsconnect)
library(DT)
library(lubridate)


article_codes <- read.csv(file = '/Users/kathrynmurenbeeld/Analysis/VIP_Wildlife_Conflict/shiny_visual/data/original/new_codes.csv')

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
    column(3, tableOutput("Publication.State")),
    column(3, tableOutput("Type.of.Conflict")),
    column(3, tableOutput("Focus.is")),
    column(3, tableOutput("Value.Orientation.1.7"))
  ),
  fluidRow(
    column(12, plotOutput("state_conflict"))
  )
)

server <- function(input, output, session) {
  selected <- reactive(article_codes %>% filter(Species == input$species))
  
  output$Publication.State <- renderTable(
    selected() %>% count(Publication.State, sort = TRUE)
  )
  output$Type.of.Conflict <- renderTable(
    selected() %>% count(Type.of.Conflict, sort = TRUE)
  )
  output$Focus.is <- renderTable(
    selected() %>% count(Focus.is, sort = TRUE)
  )
  output$Value.Orientation.1.7 <- renderTable(
    selected() %>% count(Value.Orientation.1.7., sort = TRUE)
  )
  
#  summary <- reactive({
#    selected() %>%
#      count(Value.Orientation.1.7.) 
     # left_join(population, by = c("age", "sex")) %>%
    #  mutate(rate = n / population * 1e4)
 # })
  
  output$state_conflict <- renderPlot({
    selected() %>%
      ggplot(aes(x = Value.Orientation.1.7.)) +
      geom_histogram(binwidth = 1, color="darkblue", fill="lightblue") +
      scale_x_continuous(labels = c("0" = "Mutualistic", "1" = "1", "2" = "2", "3" = "3", 
                                    "4" = "4",
                                    "5" = "5", "6" = "6", "7" = "7",  "8" = "Domination"), 
                         breaks=seq(0,8,1),
                         limits = c(0, 8)) +
      labs(x = "Distribution of Value Orientation")
  }, res = 96)
}

shinyApp(ui, server)