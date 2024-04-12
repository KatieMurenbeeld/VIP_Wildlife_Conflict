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
data$Focus[data$Focus == "Practictioner"] <- "Practitioner"
data$Focus[data$Focus == "Wildllife"] <- "Wildlife"
data$Focus <- trimws(data$Focus)
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
#ggsave(here::here("urs_figures/conflict_map.png"), conflict_map, width = 14, height = 14, dpi = 300) 

#---Northwestern States-----------

data_pnw <- data %>%
  filter(Publication_State == "WA" | Publication_State == "OR" | Publication_State == "MT" | Publication_State == "ID" | Publication_State == "WY")

# box plots with value orientation for beavers and bears and wolves
pnw_values_box <- 
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
  ggtitle("Value Orientation for Beavers, Bears, and Wolves in the PNW") +
  ylab("Value Orientation") + 
  xlab("")
pnw_values_box

ggsave(here::here("urs_figures/pnw_values_box.png"), pnw_values, width = 14, height = 14, dpi = 300) 

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
#ggsave(here::here("urs_figures/pnw_species.png"), pnw_species, width = 14, height = 14, dpi = 300) 

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
#ggsave(here::here("urs_figures/pnw_focus.png"), pnw_focus, width = 14, height = 14, dpi = 300) 

pnw_conflict <- 
  data_pnw %>%
  filter(Conflict_Type != "") %>%
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


pnw_value_bar <- 
  data_pnw %>%
  filter(!is.na(Value_Orientation)) %>%
  group_by(Species) %>%
  filter(Species == "Wolves" | Species == "Grizzly Bears" | Species == "Bison") %>%
  count(Value_Orientation, sort = TRUE) %>%
  ggplot(aes(x = as.factor(Value_Orientation), y = n, fill = Species)) +
  geom_col() +
  theme_bw() +
  theme(
    legend.position="none",
    plot.title = element_text(size=12)
  ) +
  ggtitle("Number of Articles by Value Orientation, PNW") +
  scale_x_discrete(labels=c("1" = "Mutualistic", 
                            "2" = "", 
                            "3" = "",
                            "4" = "Neutral",
                            "5" = "",
                            "6" = "",
                            "7" = "Domination")
  ) +
  ylab("Count") + 
  xlab("") + 
  facet_wrap(~Species)
pnw_value_bar

ggsave(here::here("urs_figures/pnw_value_bar.png"), pnw_value_bar, width = 14, height = 14, dpi = 300) 


#---Southeastern States-----------
se_states <- c("KY", "VA", "NC", "SC", "GA", "FL", "AL", "MS", "LA", "TX", "AR")
data_se <- data %>%
  filter(Publication_State %in% se_states)

se_values_box <- 
  data_se %>%
  filter(Species == "Alligators" | Species == "Beavers" | Species == "Boars" | Species == "Coyotes") %>%
  ggplot( aes(x=Species, y=Value_Orientation, fill=Species)) +
  geom_boxplot(notch = FALSE,
               alpha = 0.4) +
  theme_bw() +
  theme(
    legend.position="none",
    plot.title = element_text(size=12)
  ) +
  ggtitle("Value Orientation for Alligators, Beavers, Boars, and Coyotes in the SE") +
  ylab("Value Orientation") + 
  xlab("")
se_values_box
ggsave(here::here("urs_figures/se_values_box.png"), se_values_box, width = 14, height = 14, dpi = 300) 

se_species <- 
  data_se %>%
  count(Species, sort = TRUE) %>%
  ggplot(aes(x=Species, y=n, fill=Species)) +
  geom_col() +
  theme_bw() +
  theme(
    legend.position="none",
    plot.title = element_text(size=12)
  ) +
  ggtitle("Number of Articles for Each Species, SE") +
  ylab("Count") + 
  xlab("")
se_species
ggsave(here::here("urs_figures/se_species.png"), se_species, width = 14, height = 14, dpi = 300) 

se_focus <- 
  data_se %>%
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
se_focus
ggsave(here::here("urs_figures/se_focus.png"), se_focus, width = 14, height = 14, dpi = 300) 

se_conflict <- 
  data_se %>%
  count(Conflict_Type, sort = TRUE) %>%
  ggplot(aes(x=Conflict_Type, y=n, fill=Conflict_Type)) +
  geom_col() +
  theme_bw() +
  theme(
    legend.position="none",
    plot.title = element_text(size=12)
  ) +
  ggtitle("Number of Articles by Conflict, SE") +
  ylab("Count") + 
  xlab("")
se_conflict
ggsave(here::here("urs_figures/se_conflict.png"), se_conflict, width = 14, height = 14, dpi = 300) 


se_value_bar <- 
  data_se %>%
  filter(!is.na(Value_Orientation)) %>%
  group_by(Species) %>%
  filter(Species == "Alligators" | Species == "Beavers" | Species == "Boars" | Species == "Coyotes") %>%
  count(Value_Orientation, sort = TRUE) %>%
  ggplot(aes(x = as.factor(Value_Orientation), y = n, fill = Species)) +
  geom_col() +
  theme_bw() +
  theme(
    legend.position="none",
    plot.title = element_text(size=12)
  ) +
  ggtitle("Number of Articles by Value Orientation, SE") +
  scale_x_discrete(labels=c("1" = "Mutualistic", 
                            "2" = "", 
                            "3" = "",
                            "4" = "Neutral",
                            "5" = "",
                            "6" = "",
                            "7" = "Domination")
  ) +
  ylab("Count") + 
  xlab("") + 
  facet_wrap(~Species)
se_value_bar
ggsave(here::here("urs_figures/se_value_bar.png"), se_value_bar, width = 14, height = 14, dpi = 300) 

#---Northeastern States---------

ne_states <- c("PA", "NY", "NJ", "NH", "RI", "MA", "VT", "ME", "CT")
data_ne <- data %>%
  filter(Publication_State %in% ne_states)

ne_values_box <- 
  data_ne %>%
  #filter(Species == "Alligators" | Species == "Beavers" | Species == "Boars" | Species == "Coyotes") %>%
  ggplot( aes(x=Species, y=Value_Orientation, fill=Species)) +
  geom_boxplot(notch = FALSE,
               alpha = 0.4) +
  theme_bw() +
  theme(
    legend.position="none",
    plot.title = element_text(size=12)
  ) +
  ggtitle("Value Orientation for Species in NE") +
  ylab("Value Orientation") + 
  xlab("")
ne_values_box
ggsave(here::here("urs_figures/ne_values_box.png"), ne_values_box, width = 14, height = 14, dpi = 300) 

ne_species <- 
  data_ne %>%
  count(Species, sort = TRUE) %>%
  ggplot(aes(x=Species, y=n, fill=Species)) +
  geom_col() +
  theme_bw() +
  theme(
    legend.position="none",
    plot.title = element_text(size=12)
  ) +
  ggtitle("Number of Articles for Each Species, NE") +
  ylab("Count") + 
  xlab("")
ne_species
ggsave(here::here("urs_figures/ne_species.png"), ne_species, width = 14, height = 14, dpi = 300) 

ne_focus <- 
  data_ne %>%
  count(Focus, sort = TRUE) %>%
  ggplot(aes(x=Focus, y=n, fill=Focus)) +
  geom_col() +
  theme_bw() +
  theme(
    legend.position="none",
    plot.title = element_text(size=12)
  ) +
  ggtitle("Number of Articles by Focus, NE") +
  ylab("Count") + 
  xlab("")
ne_focus
ggsave(here::here("urs_figures/ne_focus.png"), ne_focus, width = 14, height = 14, dpi = 300) 

ne_conflict <- 
  data_ne %>%
  count(Conflict_Type, sort = TRUE) %>%
  ggplot(aes(x=Conflict_Type, y=n, fill=Conflict_Type)) +
  geom_col() +
  theme_bw() +
  theme(
    legend.position="none",
    plot.title = element_text(size=12)
  ) +
  ggtitle("Number of Articles by Conflict, NE") +
  ylab("Count") + 
  xlab("")
ne_conflict
ggsave(here::here("urs_figures/ne_conflict.png"), ne_conflict, width = 14, height = 14, dpi = 300) 


ne_value_bar <- 
  data_ne %>%
  filter(!is.na(Value_Orientation)) %>%
  group_by(Species) %>%
  #filter(Species == "Alligators" | Species == "Beavers" | Species == "Boars" | Species == "Coyotes") %>%
  count(Value_Orientation, sort = TRUE) %>%
  ggplot(aes(x = as.factor(Value_Orientation), y = n, fill = Species)) +
  geom_col() +
  theme_bw() +
  theme(
    legend.position="none",
    plot.title = element_text(size=12)
  ) +
  ggtitle("Number of Articles by Value Orientation, NE") +
  scale_x_discrete(labels=c("1" = "Mutualistic", 
                            "2" = "", 
                            "3" = "",
                            "4" = "Neutral",
                            "5" = "",
                            "6" = "",
                            "7" = "Domination")
  ) +
  ylab("Count") + 
  xlab("") + 
  facet_wrap(~Species)
ne_value_bar
ggsave(here::here("urs_figures/ne_value_bar.png"), ne_value_bar, width = 14, height = 14, dpi = 300) 

#---Western States--------
we_states <- c("CA", "NV", "AZ", "CO", "UT", "NM")
data_we <- data %>%
  filter(Publication_State %in% we_states)

we_values_box <- 
  data_se %>%
  #filter(Species == "Alligators" | Species == "Beavers" | Species == "Boars" | Species == "Coyotes") %>%
  ggplot( aes(x=Species, y=Value_Orientation, fill=Species)) +
  geom_boxplot(notch = FALSE,
               alpha = 0.4) +
  theme_bw() +
  theme(
    legend.position="none",
    plot.title = element_text(size=12)
  ) +
  ggtitle("Value Orientation for Species in Western US") +
  ylab("Value Orientation") + 
  xlab("")
we_values_box
ggsave(here::here("urs_figures/we_values_box.png"), we_values_box, width = 14, height = 14, dpi = 300) 

we_species <- 
  data_we %>%
  count(Species, sort = TRUE) %>%
  ggplot(aes(x=Species, y=n, fill=Species)) +
  geom_col() +
  theme_bw() +
  theme(
    legend.position="none",
    plot.title = element_text(size=12)
  ) +
  ggtitle("Number of Articles for Each Species, WE") +
  ylab("Count") + 
  xlab("")
we_species
ggsave(here::here("urs_figures/we_species.png"), we_species, width = 14, height = 14, dpi = 300) 

we_focus <- 
  data_we %>%
  count(Focus, sort = TRUE) %>%
  ggplot(aes(x=Focus, y=n, fill=Focus)) +
  geom_col() +
  theme_bw() +
  theme(
    legend.position="none",
    plot.title = element_text(size=12)
  ) +
  ggtitle("Number of Articles by Focus, WE") +
  ylab("Count") + 
  xlab("")
we_focus
ggsave(here::here("urs_figures/we_focus.png"), we_focus, width = 14, height = 14, dpi = 300) 

we_conflict <- 
  data_we %>%
  count(Conflict_Type, sort = TRUE) %>%
  ggplot(aes(x=Conflict_Type, y=n, fill=Conflict_Type)) +
  geom_col() +
  theme_bw() +
  theme(
    legend.position="none",
    plot.title = element_text(size=12)
  ) +
  ggtitle("Number of Articles by Conflict, WE") +
  ylab("Count") + 
  xlab("")
we_conflict
ggsave(here::here("urs_figures/we_conflict.png"), we_conflict, width = 14, height = 14, dpi = 300) 


we_value_bar <- 
  data_we %>%
  filter(!is.na(Value_Orientation)) %>%
  group_by(Species) %>%
  #filter(Species == "Alligators" | Species == "Beavers" | Species == "Boars" | Species == "Coyotes") %>%
  count(Value_Orientation, sort = TRUE) %>%
  ggplot(aes(x = as.factor(Value_Orientation), y = n, fill = Species)) +
  geom_col() +
  theme_bw() +
  theme(
    legend.position="none",
    plot.title = element_text(size=12)
  ) +
  ggtitle("Number of Articles by Value Orientation, WE") +
  scale_x_discrete(labels=c("1" = "Mutualistic", 
                            "2" = "", 
                            "3" = "",
                            "4" = "Neutral",
                            "5" = "",
                            "6" = "",
                            "7" = "Domination")
  ) +
  ylab("Count") + 
  xlab("") + 
  facet_wrap(~Species)
we_value_bar
ggsave(here::here("urs_figures/we_value_bar.png"), we_value_bar, width = 14, height = 14, dpi = 300) 

#---Plains and Midwest-------
mw_states <- c("OK", "SD", "ND", "MN", "WI", "MI", "IA", "IN", "MO", "IL", "MI", "OH")
data_mw <- data %>%
  filter(Publication_State %in% mw_states)

mw_values_box <- 
  data_mw %>%
  #filter(Species == "Alligators" | Species == "Beavers" | Species == "Boars" | Species == "Coyotes") %>%
  ggplot( aes(x=Species, y=Value_Orientation, fill=Species)) +
  geom_boxplot(notch = FALSE,
               alpha = 0.4) +
  theme_bw() +
  theme(
    legend.position="none",
    plot.title = element_text(size=12)
  ) +
  ggtitle("Value Orientation for Species in the MW") +
  ylab("Value Orientation") + 
  xlab("")
mw_values_box
ggsave(here::here("urs_figures/mw_values_box.png"), mw_values_box, width = 14, height = 14, dpi = 300) 

mw_species <- 
  data_mw %>%
  count(Species, sort = TRUE) %>%
  ggplot(aes(x=Species, y=n, fill=Species)) +
  geom_col() +
  theme_bw() +
  theme(
    legend.position="none",
    plot.title = element_text(size=12)
  ) +
  ggtitle("Number of Articles for Each Species, MW") +
  ylab("Count") + 
  xlab("")
mw_species
ggsave(here::here("urs_figures/mw_species.png"), mw_species, width = 14, height = 14, dpi = 300) 

mw_focus <- 
  data_mw %>%
  count(Focus, sort = TRUE) %>%
  ggplot(aes(x=Focus, y=n, fill=Focus)) +
  geom_col() +
  theme_bw() +
  theme(
    legend.position="none",
    plot.title = element_text(size=12)
  ) +
  ggtitle("Number of Articles by Focus, MW") +
  ylab("Count") + 
  xlab("")
mw_focus
ggsave(here::here("urs_figures/mw_focus.png"), mw_focus, width = 14, height = 14, dpi = 300) 

mw_conflict <- 
  data_mw %>%
  count(Conflict_Type, sort = TRUE) %>%
  ggplot(aes(x=Conflict_Type, y=n, fill=Conflict_Type)) +
  geom_col() +
  theme_bw() +
  theme(
    legend.position="none",
    plot.title = element_text(size=12)
  ) +
  ggtitle("Number of Articles by Conflict, SE") +
  ylab("Count") + 
  xlab("")
mw_conflict
ggsave(here::here("urs_figures/mw_conflict.png"), mw_conflict, width = 14, height = 14, dpi = 300) 


mw_value_bar <- 
  data_mw %>%
  filter(!is.na(Value_Orientation)) %>%
  group_by(Species) %>%
  #filter(Species == "Alligators" | Species == "Beavers" | Species == "Boars" | Species == "Coyotes") %>%
  count(Value_Orientation, sort = TRUE) %>%
  ggplot(aes(x = as.factor(Value_Orientation), y = n, fill = Species)) +
  geom_col() +
  theme_bw() +
  theme(
    legend.position="none",
    plot.title = element_text(size=12)
  ) +
  ggtitle("Number of Articles by Value Orientation, MW") +
  scale_x_discrete(labels=c("1" = "Mutualistic", 
                            "2" = "", 
                            "3" = "",
                            "4" = "Neutral",
                            "5" = "",
                            "6" = "",
                            "7" = "Domination")
  ) +
  ylab("Count") + 
  xlab("") + 
  facet_wrap(~Species)
mw_value_bar
ggsave(here::here("urs_figures/mw_value_bar.png"), mw_value_bar, width = 14, height = 14, dpi = 300) 


#---2023 URS figures-----------
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


