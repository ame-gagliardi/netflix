library(tidyverse)
library(lubridate)

titles <- as_tibble(read.delim("Data/raw/netflix/netflix_titles.csv", sep = ",", na.strings = ""))
actors <- as_tibble(read.delim("Data/raw/netflix/netflix_actors.csv", sep = ",", na.strings = ""))
extra <- as_tibble(read.delim("Data/raw/netflix_titles.csv", sep = ",", na.strings = ""))

movies <- extra %>% 
  left_join(titles, by = "title") %>% 
  select(title, type.x, country, release_year.x, date_added, rating, duration, listed_in, director, cast, imdb_score, imdb_votes) %>% 
  rename("type" = type.x, 
         "release_date" = release_year.x,
         "added_date" = date_added,
         "genres" = listed_in) %>%
  mutate(added_date = lubridate::mdy(added_date),
         release_date = as.Date(as.character(release_date), format = "%Y")) %>% 
  mutate(release_date = year(release_date))
         
genres <- movies %>% 
  dplyr::select(title, genres) %>% 
  tidyr::separate(col = genres, into = c("genre_1", "genre_2", "genre_3"), sep = ",", extra = "warn") %>% 
  dplyr::mutate(genre_1 = trimws(genre_1),
                genre_2 = trimws(genre_2),
                genre_3 = trimws(genre_3)) %>% 
  dplyr::mutate(genre_1 = ifelse(str_detect(genre_1, "and "), str_remove(genre_1, "and "), genre_1),
                genre_2 = ifelse(str_detect(genre_2, "and "), str_remove(genre_2, "and "), genre_2),
                genre_3 = ifelse(str_detect(genre_3, "and "), str_remove(genre_3, "and "), genre_3)) %>% 
  tidyr::pivot_longer(cols = c(2,3,4)) %>% 
  dplyr::select(-name) %>% 
  dplyr::filter(!(is.na(value))) %>% 
  dplyr::rename("genre" = value)

countries <- movies %>% 
  dplyr::select(title, type ,country) %>% 
  tidyr::separate(col = country, into = c("country_1", "country_2", "country_3", "country_4", "country_5", "country_6", "country_7",
                                          "country_8", "country_9"), sep = ",", extra = "warn") %>% 
  dplyr::mutate(across(.cols = c(1:11), .fns = trimws)) %>% 
  tidyr::pivot_longer(cols = c(3:11)) %>% 
  dplyr::select(-name) %>% 
  dplyr::filter(!(is.na(value))) %>% 
  dplyr::rename("country" = value)




write.table(movies, file = "Data/cleaned/netflix_movies_df.csv", sep = "\t", quote = F, row.names = F)
write.table(countries, file = "Data/cleaned/netflix_countries_df.csv", sep = "\t", quote = F, row.names = F)
write.table(genres, file = "Data/cleaned/netflix_genres_df.csv", sep = "\t", quote = F, row.names = F)
