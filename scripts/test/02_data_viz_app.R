library(tidyverse)
library(ggplot2)
library(ggmap)
library(sf)
library(tigris)
library(scales)

# Load the data

data <- read.csv("data/original/new_codes.csv")

## Clean up the Focus variable (multiple spellings of practitioner)
data$Focus[data$Focus == "Practicioner"] <- "Practitioner"
data$Focus[data$Focus == "Practioner"] <- "Practitioner"
data$Focus[data$Focus == "Practioners"] <- "Practitioner"

#################
## Patrick: 
## Map with Mean Value Orientation as State Fill

## Create data frame

state_df <- data %>%
  drop_na(Value_Orientation) %>%
  filter(Species == "Wolves") %>%
  group_by(Publication_State) %>%
  summarise(mean_value = mean(Value_Orientation), n = n(), n_article = length(unique(Article_Title)))

## Create state map variable
us_states <- states(cb = TRUE) %>%
  filter(GEOID < "60") %>%
  filter(GEOID != "02") %>%
  filter(GEOID != "15") 

## Join the 2 data frames and replace mean_values of NA with 0s
state_val <- right_join(state_df, us_states, by = c("Publication_State" = "STUSPS"))


value_map_wolves <- ggplot() +
  geom_sf(data = us_states, fill = NA, color = "black", size = 0.1) +
  geom_sf(data = state_val, aes(geometry = geometry, fill = mean_value), size = 0.05) +
  scale_fill_gradient2(na.value = "white",
    low = "yellow",
    mid = "green",
    high = "navy",
    midpoint = 4
  )
ggsave("value_map_wolves_02.png", value_map_wolves, width = 12, height = 12, dpi = 300) 
# Note to self: update labels of legend to show mutualistic at 0 and domination at 7
# Also check with Patrick about colors
  
#################
## Helia: 
## For all species compare value orientations of all species
## between the focus is practitioners and policy
## And a pie chart for Bison of Focus
## And a bar chart for conflict type and average value orientation
## (for just Bison?)

## Create dataframe

prac_policy <- data %>%
  filter(Focus == "Practitioner" | Focus == "Policy")

prac_poli_bxplt <- ggplot(prac_policy, aes(x=Species, y=Value.Orientation.1.7., fill=Focus.is)) +
  geom_boxplot(position=position_dodge(1)) 
ggsave("prac_poli_bxplt.png", prac_poli_bxplt, width = 12, height = 12, dpi = 300) 

## Create a data frame for just Bison
bison_df <- data %>%
  filter(Species == "Bison")

## Create a variable for the percentage of Focus is... 
bison_df <- bison_df %>%
  count(Focus) %>%
  mutate(percent_focus = n / sum(n))
  
bison_pie_chart <- ggplot(bison_df, aes(x="", y=percent_focus, group=Focus, color=Focus, fill=Focus)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid  = element_blank())  
ggsave("bison_pie_chart.png", bison_pie_chart, width = 12, height = 12, dpi = 300) 


#################
## Mackenzie: 
## For bears create 3 pie charts of Focus is
## For Montana, Washington, and all other states combined

## Create dataframe
  
bear_pie <- data %>%
  filter(Species == "Grizzly Bear")
bear_pie$Publication.State[bear_pie$Publication_State != "MT" & bear_pie$Publication_State != "WA"] <- "Other"

## Create variable for the percentage of Focus.is for MT, WA, and other
bear_pie <- bear_pie %>%
  group_by(Publication_State) %>%
  count(Focus) %>%
  mutate(percent_focus = n / sum(n))

pie_chart <- ggplot(bear_pie, aes(x="", y=percent_focus, group=Focus.is, color=Focus.is, fill=Focus.is)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) + facet_wrap(~ Publication.State) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid  = element_blank())  
ggsave("pie_chart.png", pie_chart, width = 12, height = 12, dpi = 300) 

  
  
  