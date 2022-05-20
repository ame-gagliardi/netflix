library(tidyverse)


titles <- as_tibble(read.delim("Data/raw/netflix/netflix_titles.csv", sep = ",", na.strings = ""))
actors <- as_tibble(read.delim("Data/raw/netflix/netflix_actors.csv", sep = ",", na.strings = ""))
extra <- as_tibble(read.delim("Data/raw/netflix_titles.csv", sep = ",", na.strings = ""))

movies <- extra %>% 
  left_join(titles, by = "title") %>% 
  select(title, type.x, country, release_year.x, date_added, rating, duration, listed_in, director, cast, imdb_score, imdb_votes) %>% 
  rename(type.x = "type", 
         release_year.x = "release_date",
         date_added = "added_date",
         listed_in = "genres") %>% 
  mutate(added_date = lubridate::mdy(added_date),
         release_date = as.Date(as.character(movies$release_date), format = "%Y")) %>% 
  mutate(release_data = )


movies <- extra %>% 
  left_join(titles, by = "title") %>% 
  select(title, type.x, country, release_year.x, date_added, rating, duration, listed_in, director, cast, imdb_score, imdb_votes) %>% 
  rename(type.x = "type", 
         release_year.x = "release_date",
         date_added = "added_date",
         listed_in = "genres") %>% 
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
                genre_3 = ifelse(str_detect(genre_3, "and "), str_remove(genre_3, "and "), genre_3))


write.table(movies, file = "Data/cleaned/netflix_movies_df.csv", sep = "\t", quote = F, row.names = F)
