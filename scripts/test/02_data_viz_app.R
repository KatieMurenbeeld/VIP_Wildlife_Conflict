library(tidyverse)
library(ggplot2)
library(ggmap)
library(sf)
library(tigris)

# Load the data

data <- read.csv("data/original/new_codes.csv")

#################
## Patrick: 
## Map with Mean Value Orientation as State Fill

## Create data frame

state_df <- data %>%
  drop_na(Value.Orientation.1.7.) %>%
  filter(Species == "Wolves") %>%
  group_by(Publication.State) %>%
  summarise(mean_value = mean(Value.Orientation.1.7.), n = n(), n_article = length(unique(Article.Title)))

## Create stat map variable
us_states <- states(cb = TRUE) %>%
  filter(GEOID < "60") %>%
  filter(GEOID != "02") %>%
  filter(GEOID != "15") 

value_map_wolves <- ggplot() +
  geom_sf(data = us_states, fill = NA, color = "black", size = 0.1) + 
  geom_sf(data = state_df, aes(fill = mean_value), size = 0.05) +
  scale_fill_continuous()
  
  
  
  
  
  
  
  
  
  