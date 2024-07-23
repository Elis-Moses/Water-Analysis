##WATER SOURCES ANALYSIS WITH R

install.packages('tidytuesdayR')
install.packages('ggthemes')
install.packages('ggmap')
install.packages("gganimate")

library(gganimate)
library(ggmap)
library(tidyverse)
library(tidytuesdayR)
library(scales)
library(countrycode)
library(ggplot2)
library(ggthemes)
theme_set(theme_light())

#Gathering the data from Tidy Tuesday
tt <- tt_load('2021-05-04')
view(tt$water)

#Checking Countries without data
tt$water %>%
  count(country_name, sort = TRUE)

##Cleaning the data
water <- tt$water %>%
#changing the date pattern
  mutate(report_date = mdy(report_date)) %>%
#renaming columns
  rename(lat = lat_deg,
         lon = lon_deg,
         country = country_name) %>%
#separating water tech from the brand name
  separate(water_tech, c('water_tech', 'brand'), sep = '-', fill = 'right') %>%
#removing countries without data
  filter(!country %in% c('Peru', 'Dominican Republic', 'Timor-leste'), !is.na(country)) %>%
#removing data from 2021 on wards 
  mutate(install_year = ifelse(install_year > 2021, NA_real_, install_year)) %>%
  filter(between(lat, -35, 37),
         between(lon, -40, 60))

#Visualizing the countries in the data using geom points
water %>%
  sample_n(10000) %>%
  ggplot(aes(lat,lon, colour = country)) +
  geom_point()

#Adding country labeling to the points 
water %>%
  group_by(country) %>%
  summarise(lat = mean(lat),
            lon = mean(lon)) %>%
  ggplot(aes(lon, lat)) +
  geom_point()+
  geom_text(aes(label = country), vjust = 1, hjust = 1)
  
library(countrycode)
countries <- unique(water$country)

#Getting Africa map from world map
africa_map_data <- map_data("world") %>%
  as_tibble() %>%
  mutate(continent = countrycode(region, "country.name", "continent")) %>%
  filter(continent =="Africa")

#Plotting water sources using Africa map
water %>%
  sample_n(1000) %>%
  ggplot(aes(lon, lat)) +
  geom_polygon(aes(long, lat, group = group),
               color = "grey",
               fill = "white",
               data = africa_map_data,
               size = .25)+
  geom_point(size = .1, alpha = .25)+
  theme_map()

#Determining the latitude and longitude of a Uganda using point
water %>%
  filter(country == "Uganda") %>%
  ggplot(aes(lon, lat)) +
  geom_point()

#Visualizing water sources in Uganda with point
water_uganda <- water %>%
  filter(country == "Uganda",
         between(lat, -2, 5),
         between(lon, 29, 40))

water_uganda %>%
  ggplot(aes(lon, lat)) +
  geom_point()

#Having a more clearer visualization of water sources in Uganda
water_uganda %>%
  ggplot(aes(lon, lat, colour = status_id)) +
  borders("world", region = "Uganda") +
  geom_point(size = .1, alpha = .25) +
  theme_map() +
  scale_color_discrete(guide = guide_legend(override.aes = list(size =2, alpha(1))))


##Visualizing water sources using stadia maps

#Registering stadia maps API keys
register_stadiamaps(key = "63e02dde-3618-4727-a17c-b49445e5a09b")

#Defining bounding box of Uganda
bbox <- c(left = 29.573433, bottom = -1.48192, right = 35.000308, top = 4.234762)

#Getting the Map
uganda_map <- get_stadiamap(bbox, zoom = 7)

#Viewing Uganda map
ggmap(uganda_map)

#Determining the top 5 water sources in Uganda
water_uganda_lump <- water_uganda %>%
  mutate(water_source = fct_lump(water_source, 5)) %>%
  replace_na(list(water_source = "Other")) %>%
  mutate(water_source = fct_reorder(water_source, water_source, length, .desc = TRUE))
View(water_uganda)

#Using facet wrap to visualize the top 5 water sources in Uganda
ggmap(uganda_map) +
  geom_point(aes(lon, lat),
             data = water_uganda_lump, size = .1, alpha = .1) +
  facet_wrap(~ water_source)


#plotting the installation years of water sources using Uganda map
ggmap(uganda_map) +
  geom_point(aes(lon,lat,color = install_year),
             data = water_uganda %>% sample_n(20000),
             size = .2)+
  scale_color_gradient2(low = "#ACD4EC", high = "#2A5783",
                        midpoint = 1990)

#Getting precise water sources in Uganda from 1990 - 2021
point_uganda <- water_uganda %>%
  filter(!is.na(install_year)) %>%
  sample_n(10000) %>%
  mutate(install_year = pmax(1990, install_year)) %>%
  mutate(year = map(install_year, ~seq(., 2021))) %>%
  unnest(year)
  
#Plotting a transitioning map of water sources in Uganda from 1990 - 2021 
ggmap(uganda_map) +
  geom_point(aes(lon, lat), data =point_uganda, size = .1, alpha = .25) +
  transition_manual(year) +
  labs(title = "water sources in uganda in year : {current_frame}")
