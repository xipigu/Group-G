
old_data <- read.csv("/Drafts/movie_metadata.csv")
head(old_data)
filter_data <- filter(old_data, country=="USA", language=="English")
head(filter_data)
select_data <- select(filter_data, movie_title, title_year, director_name, duration, gross, genres, num_voted_users, num_user_for_reviews, budget, imdb_score, movie_facebook_likes, actor_1_name, actor_2_name, actor_3_name)
head(select_data)
select_data_filter <- filter(select_data, title_year >= 2011, gross !=0 | gross != "NA" | gross != "", imdb_score != 0 | imdb_score != "NA" | imdb_score != "",budget != 0 | budget != "NA" | budget != "", movie_facebook_likes != 0 | movie_facebook_likes != "NA" | movie_facebook_likes != "")
head(select_data_filter)
library(tidyr)


