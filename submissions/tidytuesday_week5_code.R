# Read about it here https://www.census.gov/hhes/www/housing/resseg/multigroup_entropy.pdf
library(dplyr)

# Grab the data
acs <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/acs2015_county_data.csv")

# Calculate county level diversity using Theil's entropy score
vars = c("Hispanic", "White", "Black", "Native", "Asian", "Pacific")
acs <- acs %>% 
  mutate_at(vars(vars), function(x) x/100) %>% 
  group_by(CensusId) %>% 
  # Calculate state level populations by group
  mutate(Hispanic_c = Hispanic*TotalPop, 
         White_c = White*TotalPop,
         Black_c = Black*TotalPop,
         Native_c = Native*TotalPop,
         Asian_c = Asian*TotalPop,
         Pacific_c = Pacific*TotalPop) %>% 
  ungroup() %>% 
  group_by(State) %>% 
  # Calculate state level population shares
  mutate(Pop_s = sum(TotalPop),
         Hispanic_s = sum(Hispanic_c)/sum(TotalPop), 
         White_s = sum(White_c)/sum(TotalPop),
         Black_s = sum(Black_c)/sum(TotalPop),
         Native_s = sum(Native_c)/sum(TotalPop),
         Asian_s = sum(Asian_c)/sum(TotalPop),
         Pacific_s = sum(Pacific_c)/sum(TotalPop)) %>% 
  ungroup() %>% 
  group_by(CensusId) %>% 
  # Calculate county level entropy score
  mutate(E = sum(Hispanic*log(1/Hispanic)+
                   White*log(1/White),
                 Black*log(1/Black),
                 Native*log(1/Native),
                 Asian*log(1/Asian),
                 Pacific*log(1/Pacific), na.rm = TRUE),
         # Calculate state level entropy score
         Es = sum(Hispanic_s*log(1/Hispanic_s),
                  White_s*log(1/White_s),
                  Black_s*log(1/Black_s),
                  Native_s*log(1/Native_s),
                  Asian_s*log(1/Asian_s),
                  Pacific_s*log(1/Pacific_s), na.rm = TRUE),
         # Calculate population weighted entropy difference
         Ed = (TotalPop*(Es-E)/(Es*Pop_s)))  %>% 
  ungroup() %>% 
  #Calculate entropy index of state segregation %>% 
  group_by(State) %>% 
  mutate(H = sum(Ed, na.rm = TRUE),
         AvgE = mean(E, na.rm = TRUE)) %>% 
  ungroup()

# Visualize
#devtools::install_github("hrbrmstr/statebins")
library(ggplot2)
library(statebins)
library(ggalt)

# State-level summaries
acs_state <- acs %>% select(State, Es, AvgE, H) %>% 
  distinct() %>% 
  mutate(brks = cut(-H, breaks = c(01, -0.05,-0.1,-0.15,-0.2,-0.25), 
                    labels = c("Most\nsegregated"," ","Medium","  ","Least\nsegregated"), 
                    right = TRUE))

# Plot statebin
statebin <- ggplot(acs_state, aes(state = State, fill = brks)) + 
  geom_statebins(border_col = "grey92", lbl_size = 4) +
  scale_fill_viridis_d("",option = "magma", guide = guide_legend(direction = "horizontal", label.position = "top", override.aes = list(size = 3), keywidth = unit(3,"line"))) +
  theme_void() + 
  theme(legend.position = c(0.3,0.9), 
        plot.title = element_text(size = 28, face = "bold", family = "IBM Plex Sans"),
        plot.subtitle = element_text(size = 12, family = "IBM Plex Sans"),
        plot.background = element_rect(fill = "grey92")) +
  # Text alignment is always wonky with grid arrangements
  labs(title = "     Diverse counties and segregated states",
       subtitle = "             Estimating state-level racial segregation from county-level population data\n             using Theil's H, or the Multigroup Entropy Index")

# Plot ranking 
div_rank <- ggplot(acs_state ,aes(x = reorder(State,AvgE), y = AvgE, colour = brks)) +
  geom_lollipop(point.size = 2) + 
  coord_flip() + 
  labs(title = "State-level county diversity entropy score", 
       subtitle = "Coloured by degree of intra-county segregation",
       x ="",y ="Average of county entropy",
       caption = "@dshkol - For #tidytuesday week5 - Data: American Community Survey, 2015") +
  guides(colour = guide_legend(override.aes = list(size=5))) +
  scale_colour_viridis_d("",option = "magma", direction = -1) +
  theme(panel.background = element_rect(fill = "grey92"),
        plot.background = element_rect(fill = "grey92"),
        panel.grid = element_blank(),
        plot.title = element_text(size = 20, face = "bold", family = "IBM Plex Sans",
                                  hjust = -0.6),
        plot.subtitle = element_text(size = 12, family = "IBM Plex Sans", hjust = -0.27),
        legend.background = element_blank(),
        legend.position = c(0.8,0.3),
        legend.key = element_blank(),
        legend.key.size = unit(2,"line"))

# Layout
library(grid)
library(gridExtra)

plot <- grid.arrange(statebin, div_rank, ncol = 1)
ggsave("tidytuesday.png", plot = plot, width = 8.5, height = 11, units = "in", dpi = 300)