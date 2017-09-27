##Initial cleaning

library(dplyr)
library(readr)
library(tidyr)
library(magrittr)
library(stringr)

old_data <- read.csv("./Drafts/movie_metadata.csv")
head(old_data)
filter_data <- filter(old_data, country=="USA", language=="English")
#head(filter_data)
select_data <- select(filter_data, movie_title, title_year, director_name, duration, gross, genres, num_voted_users, num_user_for_reviews, budget, imdb_score, movie_facebook_likes, actor_1_name, actor_2_name, actor_3_name)
#head(select_data)
select_data_filter <- filter(select_data, title_year >= 2011, gross !=0, gross != "NA", gross != "", imdb_score != 0, imdb_score != "NA", imdb_score != "", budget != 0, budget != "NA", budget != "", movie_facebook_likes != 0, movie_facebook_likes != "NA", movie_facebook_likes != "")

#head(select_data_filter)

##Genre conversion

typeof(select_data_filter$genres)
toString(select_data_filter$genres)
str(select_data_filter)
genre_names <- c("Action","Adventure", "Animation", "Biography", "Comedy", "Crime", "Documentary", "Drama", "Family", "Fantasy", "History", "Horror", "Musical", "Mystery", "News", "Romance", "Sci-Fi", "Sport", "Thriller", "War", "Western")
genre_types <- c("genre1", "genre2","genre3", "genre4", "genre5", "genre6", "genre7")

genre_filter <- select_data_filter %>% separate(genres, into=genre_types,sep = "\\|")
#head(genre_filter)

##Establishing a Gross Revenue / Budget variable and cleaning up movie titles

RB_data <- mutate(.data = genre_filter, return = gross / budget)
RB_data$return <- round(RB_data$return, 2)
head(RB_data)
RB_data$movie_title <- gsub("Â","",RB_data$movie_title)
RB_data$movie_title = substr(RB_data$movie_title,1,nchar(RB_data$movie_title)-1)
head(RB_data)

##Introducing flags for genres
length(RB_data$movie_title)
<<<<<<< HEAD
RB_data$genre1 <- gsub("NA","0",RB_data$genre1)
RB_data$actiondummy <- as.numeric( RB_data$genre1 == "Action" | RB_data$genre2 == "Action" | RB_data$genre3 == "Action" | RB_data$genre4 == "Action" | RB_data$genre5 == "Action" | RB_data$genre6 == "Action" | RB_data$genre7 == "Action")


=======
row <- grep(RB_data$movie_title,"The Dark Knight ")
as.character(RB_data$movie_title[1]) == as.character("The Dark Knight Rises ")
head(RB_data)
typeof(RB_data$genre1)
typeof(RB_data$movie_title)
RB_data$movie_title[1] == as.character("The Dark Knight Rises ")

#to search for a string in any column
str_detect(RB_data$movie_title[2],"John Carter")
>>>>>>> 72fdf9dd66e36ee6059b100f19600127b9f51367

##Establishing separate datasets for time periods

data11_15 <- filter(.data = RB_data, title_year < 2016)
#head(data11_15)
data16 <- filter(.data = RB_data,title_year == 2016)
#head(data16)
print(data11_15$movie_facebook_likes)