library(tidyverse)
library(ggplot2)
library(ggmap)
library(ggpie)
library(sf)
library(tigris)
library(scales)

# Load the data

data <- read.csv("data/original/new_codes.csv")

## Clean up the Focus variable (multiple spellings of practitioner)
data$Focus[data$Focus == "Practicioner"] <- "Practitioner"
data$Focus[data$Focus == "Practioner"] <- "Practitioner"
data$Focus[data$Focus == "Practioners"] <- "Practitioner"
data$Species[data$Species == "Grizzly Bear"] <- "Grizzly Bears"
data$Species[data$Species == "Other"] <- "Coyotes"
data <- data %>%
  filter(Species != "Bob Cats")
data$Conflict_Type <- trimws(data$Conflict_Type)
data$Conflict_Type[data$Conflict_Type == "H-H"] <- "Human-Human"
data$Conflict_Type[data$Conflict_Type == "H-W"] <- "Human-Wildlife"
data$Conflict_Type[data$Conflict_Type == "N-W"] <- "Nature-Wildlife"
data$Conflict_Type[data$Conflict_Type == "Unstated Conflict"] <- "Unstated"




#################
## Patrick: 
## Map with Mean Value Orientation as State Fill

## Create data frame

state_df <- data %>%
  drop_na(Value_Orientation) %>%
  #filter(Species == "Wolves") %>%
  group_by(Publication_State) %>%
  summarise(mean_value = mean(Value_Orientation), n = n(), n_article = length(unique(Title)))

## Create state map variable
us_states <- states(cb = TRUE) %>%
  filter(GEOID < "60") %>%
  filter(GEOID != "02") %>%
  filter(GEOID != "15") 

## Join the 2 data frames and replace mean_values of NA with 0s
state_val <- right_join(state_df, us_states, by = c("Publication_State" = "STUSPS"))


conflict_map <- ggplot() +
  geom_sf(data = us_states, fill = NA, color = "black", size = 0.1) +
  geom_sf(data = state_val, aes(geometry = geometry, fill = n_article), size = 0.05) +
  scale_fill_gradient2(name = "Number of Articles", 
                       na.value = "white",
                       low = "lightgrey",
                       mid = "lightblue",
                       high = "navy",
                       midpoint = 20,
                       breaks = c(1,20,40,60),
                       #labels = c("Mutualist","Nuetral", "Domination")
                       ) + 
  labs(title = "Reported Conflict, n = 481",
       subtitle = "Number of Unique Articles") +
 # scale_fill_discrete(name = "Mean Value Orientation") +
  theme(plot.title = element_text(size=24),
        ) +
  theme_bw()

conflict_map
ggsave(here::here("urs_figures/conflict_map.png"), conflict_map, width = 14, height = 14, dpi = 300) 

data_pnw <- data %>%
  filter(Publication_State == "WA" | Publication_State == "OR" | Publication_State == "MT" | Publication_State == "ID")

# box plots with value orientation for beavers and bears and wolves
pnw_values <- 
  data_pnw %>%
  filter(Species == "Grizzly Bears" | Species == "Wolves" | Species == "Beavers") %>%
  ggplot( aes(x=Species, y=Value_Orientation, fill=Species)) +
  geom_boxplot(notch = FALSE,
               alpha = 0.4) +
  theme_bw() +
  theme(
    legend.position="none",
    plot.title = element_text(size=12)
  ) +
  #scale_y_discrete(labels=c("1" = "Mutualistic", 
  #                          "4" = "Neutral",
  #                          "7" = "Domination")
  #                 ) +
  ggtitle("Value Orientation for Beavers, Bears, and Wolves in the PNW") +
  ylab("Value Orientation") + 
  xlab("")
pnw_values

ggsave(here::here("urs_figures/pnw_values.png"), pnw_values, width = 14, height = 14, dpi = 300) 

pnw_species <- 
  data_pnw %>%
  count(Species, sort = TRUE) %>%
  ggplot(aes(x=Species, y=n, fill=Species)) +
  geom_col() +
  theme_bw() +
  theme(
    legend.position="none",
    plot.title = element_text(size=12)
  ) +
  ggtitle("Number of Articles for Each Species, PNW") +
  ylab("Count") + 
  xlab("")
pnw_species
ggsave(here::here("urs_figures/pnw_species.png"), pnw_species, width = 14, height = 14, dpi = 300) 

pnw_focus <- 
  data_pnw %>%
  count(Focus, sort = TRUE) %>%
  ggplot(aes(x=Focus, y=n, fill=Focus)) +
  geom_col() +
  theme_bw() +
  theme(
    legend.position="none",
    plot.title = element_text(size=12)
  ) +
  ggtitle("Number of Articles by Focus, PNW") +
  ylab("Count") + 
  xlab("")
pnw_focus
ggsave(here::here("urs_figures/pnw_focus.png"), pnw_focus, width = 14, height = 14, dpi = 300) 

pnw_conflict <- 
  data_pnw %>%
  count(Conflict_Type, sort = TRUE) %>%
  ggplot(aes(x=Conflict_Type, y=n, fill=Conflict_Type)) +
  geom_col() +
  theme_bw() +
  theme(
    legend.position="none",
    plot.title = element_text(size=12)
  ) +
  ggtitle("Number of Articles by Conflict, PNW") +
  ylab("Count") + 
  xlab("")
pnw_conflict
ggsave(here::here("urs_figures/pnw_conflict.png"), pnw_conflict, width = 14, height = 14, dpi = 300) 


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
bison_pie_df <- bison_df %>%
  count(Focus) %>%
  arrange(desc(n)) %>%
  mutate(percent_focus = (n / sum(n))*100) %>%
  mutate(ypos = cumsum(percent_focus)- 0.5*percent_focus )

## Create a variable for labels
bison_pie_df$label <- paste(bison_pie_df$Focus, paste(round(bison_pie_df$percent_focus, 0),"%"))

## Create the pie chart  
bison_pie_chart <- ggplot(bison_pie_df, aes(x="", y=percent_focus, group=Focus, color="white", fill=Focus)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) +
  theme(axis.ticks = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(color='black'),
        panel.grid  = element_blank()) +
  #scale_y_continuous(labels= bison_pie_df$label) +
  labs(title = "Bison: Proportion of Focus",
       x = "",
       y = "") +
  theme_bw()+
  theme(plot.title = element_text(size=24),
        strip.text.x = element_text(size=18),
        axis.title = element_text(size=24),
        axis.ticks = element_blank(),
        panel.grid  = element_blank(),
        legend.position = "none") +
  
  #geom_text(aes(y = ypos, label = Focus), color = "white", size=6) +
  geom_text(aes(y = ypos, label = label), color = "white", size =6) +
  scale_fill_brewer(palette="Set1")

ggsave("bison_pie_pres.png", bison_pie_chart, width = 12, height = 12, dpi = 300) 

## Calculate the average value orientation by Conflict Type
bison_mean_val <- bison_df %>%
  group_by(Conflict_Type) %>%
  summarise(mean_value = mean(Value_Orientation), n = n())


## Create the bar plot
bison_barplot <- ggplot(bison_mean_val, aes(x=Conflict_Type, y=mean_value,
                                            color = Conflict_Type,
                                            fill = Conflict_Type)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  ylim(0,8) +
  labs(title = "Bison: Value Orientation by Conflict Type",
       x = "Conflict Type",
       y = "Mean Value Orientation") +
  theme_bw() +
  theme(plot.title = element_text(size = 24),
        axis.text = element_text(size = 18),
        axis.title = element_text(size = 24))

ggsave("bison_bar_pres.png", bison_barplot, width = 12, height = 12, dpi = 300)

#################
## Mackenzie: 
## For bears create 3 pie charts of Focus is
## For Montana, Washington, and all other states combined

## Create dataframe

bear_df <- data %>%
  filter(Species == "Grizzly Bear")
  
bear_pie <- data %>%
  filter(Species == "Grizzly Bear")
bear_pie$Publication_State[bear_pie$Publication_State != "MT" & bear_pie$Publication_State != "WA"] <- "Other"

## Create variable for the percentage of Focus.is for MT, WA, and other
bear_pie <- bear_pie %>%
  group_by(Publication_State) %>%
  count(Focus) %>%
  mutate(percent_focus = (n / sum(n)) * 100)

pie_chart <- ggplot(bear_pie, aes(x="", y=percent_focus, group=Focus, color=Focus, fill=Focus)) +
  geom_bar(width = 1, stat = "identity", show.legend = TRUE) +
  coord_polar("y", start=0) + facet_wrap(~ Publication_State) +
  labs(title = "Grizzly Bear: Difference in Proportion of Focus for MT, WA, and Rest of USA",
       x = "", 
       y = "") +
  theme_bw() +
  theme(plot.title = element_text(size=24),
        strip.text.x = element_text(size=18),
        axis.title = element_text(size=24),
        axis.ticks = element_blank(),
        panel.grid  = element_blank())  +
  scale_fill_brewer(palette="Set1")

ggsave("bear_pie_pres.png", pie_chart, width = 12, height = 12, dpi = 300) 

## Calculate the average value orientation by Conflict Type
bear_mean_val <- bear_df %>%
  group_by(Conflict_Type) %>%
  summarise(mean_value = mean(Value_Orientation), n = n())

## Create the bar plot for Bears
bear_barplot <- ggplot(bear_mean_val, aes(x=Conflict_Type, y=mean_value,
                                            color = Conflict_Type,
                                            fill = Conflict_Type)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  ylim(0,8) +
  labs(title = "Grizzly Bear: Value Orientation by Conflict Type",
       x = "Conflict Type",
       y = "Mean Value Orientation") +
  theme_bw() +
  theme(plot.title = element_text(size = 24),
        axis.text = element_text(size = 18),
        axis.title = element_text(size = 24))

ggsave("bear_bar_pres.png", bear_barplot, width = 12, height = 12, dpi = 300)

#### Testing ggpie
ggpie(data = bear_pie, group_key = "Focus", count_type = "full", label_type = "none")


