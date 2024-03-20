rm(list=ls())
library(tidyverse)
library(ggrepel)
library(ggplot2)
library(dplyr)
library(raster)
library(rgdal)
library(rnaturalearthhires)
library(rnaturalearth)
library(sp)
library(broom)



world <- map_data('world')
world <- world %>% filter(region != 'Antarctica')

#world-wide policy
all_countries <- c('Belgium', 'Netherlands')

state_countries <- c('UK', 'Finland', 'Norway','France',  'Estonia', 'Germany', 'Denmark', 'Italy', 
                     'Switzerland', 'Slovenia', 'Lithuania', 'Iceland', 'Sweden')

self_countries <- c('Brazil', 'Spain', 'Greece', 'Slovakia', 'Russia', 'Czech Republic', 
                    'Poland', 'Romania', 'Cyprus', 'South Africa', 'Iran', 'Malaysia', 
                    'Singapore', 'Brunei', 'Vietnam', 'Saudi Arabia', 'Lebanon', 'Israel', 
                    'India', 'Japan', 'China', 'Australia', 'New Zealand','Mexico')

#in states policy
us_states <- map_data("state")
selected_states <- us_states %>% 
  filter(region %in% c("alabama", "arizona", "arkansas", "kansas", "massachusetts", "mississippi", "missouri", "montana", "oklahoma", "south carolina", "south dakota", "vermont", "washington"))
selected_all_states <- us_states %>% 
  filter(region %in% c("alaska", "california", "connecticut", "louisiana","colorado", "delaware", "district of columbia", "florida", "georgia", "hawaii", "idaho", "illinois", "indiana", "iowa", "kentucky", "maine", "maryland", "michigan", "minnesota", "new hampshire", "new jersey", "new mexico", "new york", "north carolina", "north dakota", "ohio", "oregon", "pennsylvania", "rhode island", "tennessee", "texas", "virginia", "west virginia", "wisconsin", "wyoming"))
left_all_states <- us_states %>% 
  filter(region %in% c("utah","nevada", "nebraska"))
usa <- ne_states(country = "United States of America", returnclass = "sf")
alaska_hawaii <- usa[usa$postal %in% c("AK", "HI"), ]

#policy in canada
canada_provinces <- ne_states(country = "Canada", returnclass = "sf")
selected_provinces <- canada_provinces[canada_provinces$name %in% c("Ontario", "Québec", "British Columbia", "Alberta", "Manitoba", "Maritimes", "Yukon", "Nunavut", "Northwest Territories"), ]


world_map <- world %>%
  mutate(
    category = case_when(
      region %in% all_countries  ~ 'ALL',
      region %in% self_countries  ~ 'SELF',
      region %in% state_countries ~ 'STATE',
      TRUE ~ 'OTHER'
    ),
    color = case_when(
      region %in% all_countries ~ "#151E5A",
      region %in% self_countries ~ '#BBC9D1',
      region %in% state_countries ~ '#326685',
      TRUE ~ '#DEE1E4'  
    )
  )



p_world_map <- ggplot(data=world_map) +
  geom_polygon(aes(x = long, y = lat, group = group, fill = color),
               colour = "white") +
  scale_fill_identity(guide="legend", name="",
                      breaks=c("#151E5A", "#326685", "#BBC9D1"),
                      labels=c("Supported by Healthcare System (General Risk)", "Supported by Healthcare System (High risk)", "Self-paid"))+
  scale_color_brewer(palette='Dark2', name='Coordination') +
  theme(legend.position = "top") +
  xlab('') +
  ylab('') +
  geom_polygon(data = selected_states, aes(x = long, y = lat, group = group), fill = "#326685", color = "white", linewidth = 0.01) +
  geom_polygon(data = selected_all_states, aes(x = long, y = lat, group = group), fill = "#151E5A", color = "white", linewidth = 0.01) +
  geom_polygon(data = left_all_states, aes(x = long, y = lat, group = group), fill = "#BBC9D1", color = "white", linewidth = 0.01) +
  geom_sf(data = alaska_hawaii, aes(fill = "#151E5A"), color = "white", linewidth = 0.01) +
  geom_sf(data = selected_provinces, aes(fill = "#326685"), color = "white", linewidth = 0.01) +
  theme(panel.background = element_blank(),
        plot.background = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_blank()) 

plot(p_world_map)

ggsave('/Users/shiyue/Desktop/world_map.pdf', p_world_map, width=27, height=15)



