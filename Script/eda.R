# Netflix EDA

library(tidyverse)
library(lubridate)
library(data.table)
library(ggrepel)
library(rgdal)
library(broom)
library(geojsonio)
library(rworldmap)

# Data loading

movies <- as_tibble(fread("Data/cleaned/netflix_movies_df.csv"))
genres <- as_tibble(fread("Data/cleaned/netflix_genres_df.csv"))
countries <- as_tibble(fread("Data/cleaned/netflix_countries_df.csv"))

#########
## EDA ##
#########

# Content type

movies %>%
  dplyr::group_by(type) %>% 
  dplyr::summarise(count = n()) %>% 
  ggplot(aes(x = type, y = count, fill = type)) + 
  geom_bar(position = "dodge", stat = "identity") +
  geom_text(aes(x = type, label = count), position = position_dodge(0.9), vjust = -0.2) +
  ggtitle("Netflix content by type") +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "none",
        plot.title = element_text(hjust = 0.5))

# Content type over time

movies %>% 
  dplyr::group_by(type, added_date) %>%
  dplyr::summarise(count = n()) %>% 
  dplyr::arrange(added_date) %>% 
  dplyr::mutate(cs = cumsum(count)) %>% 
  ggplot(aes(x = added_date, y = cs, color = type)) + 
  geom_line(size = 1.3) +
  scale_x_date(date_breaks = "6 month", date_labels = "%m/%y") +
  scale_color_discrete(name = "Content type") +
  ggtitle("Netflix content over time by type") +
  theme_bw() + 
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5))

# Distribution of film by release date

ggplot(movies, aes(x = release_date, fill = type)) +
  geom_density(alpha = 0.4) +
  theme_bw() +
  ggtitle("Distribution of film release date") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5))


  
# Clorophlet map of production #

# Aggregate count of movies for country. 

film_countries <- countries %>% 
  group_by(type, country) %>% 
  summarise(count = n()) %>% 
  arrange(desc(count))

#===================================== Renaming Country names and deleting old country names =========================================
film_countries[film_countries$country == "United States", "country"] <- "United States of America"
film_countries[film_countries$country == "Hong Kong", "country"] <- "Hong Kong S.A.R."
film_countries[film_countries$country == "Serbia", "country"] <- "Republic of Serbia"
film_countries[film_countries$country == "Bahamas", "country"] <- "The Bahamas"
film_countries[film_countries$country == "Vatican City", "country"] <- "Vatican"

toRM <- which(film_countries$country == "" | film_countries$country == "West Germany" | film_countries$country == "East Germany" |
              film_countries$country == "Palestine" | film_countries$country == "Soviet Union")
film_countries <- film_countries[-toRM, ]
#=====================================================================================================================================

# Create the world map

worldmap <- getMap(resolution = "coarse")
worldmap_df <- tidy(worldmap)

# Merge the two dataframe

film_country_map<- worldmap_df %>% 
  left_join(film_countries, by = c("id" = "country")) 
# xx <- which(is.na(film_country_map$type))
# film_country_map[xx[1:(length(xx)/2)], "type"] <- "Movie"
# film_country_map[xx[(length(xx)/2+1):length(xx)], "type"] <- "TV Show"

# Plotting the map with the layer of movies

ggplot() +
  geom_polygon(data = film_country_map, 
               aes(x=long, y=lat, group = group, fill = log(count)), color = "white") +
  scale_fill_viridis_b() +
  theme_void() +
  ggtitle("Numbe")
         
