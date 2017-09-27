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
RB_data$actiondummy <- as.numeric( RB_data$genre1 == "Action" | RB_data$genre2 == "Action" | RB_data$genre3 == "Action" | RB_data$genre4 == "Action" | RB_data$genre5 == "Action" | RB_data$genre6 == "Action" | RB_data$genre7 == "Action")
RB_data$actiondummy[is.na(RB_data$actiondummy)] <- 0

RB_data$adventuredummy <- as.numeric( RB_data$genre1 == "Adventure" | RB_data$genre2 == "Adventure" | RB_data$genre3 == "Adventure" | RB_data$genre4 == "Adventure" | RB_data$genre5 == "Adventure" | RB_data$genre6 == "Adventure" | RB_data$genre7 == "Adventure")
RB_data$adventuredummy[is.na(RB_data$adventuredummy)] <- 0

RB_data$animationdummy <- as.numeric( RB_data$genre1 == "Animation" | RB_data$genre2 == "Animation" | RB_data$genre3 == "Animation" | RB_data$genre4 == "Animation" | RB_data$genre5 == "Animation" | RB_data$genre6 == "Animation" | RB_data$genre7 == "Animation")
RB_data$animationdummy[is.na(RB_data$animationdummy)] <- 0

RB_data$biographydummy <- as.numeric( RB_data$genre1 == "Biography" | RB_data$genre2 == "Biography" | RB_data$genre3 == "Biography" | RB_data$genre4 == "Biography" | RB_data$genre5 == "Biography" | RB_data$genre6 == "Biography" | RB_data$genre7 == "Biography")
RB_data$biographydummy[is.na(RB_data$biographydummy)] <- 0

RB_data$comedydummy <- as.numeric( RB_data$genre1 == "Comedy" | RB_data$genre2 == "Comedy" | RB_data$genre3 == "Comedy" | RB_data$genre4 == "Comedy" | RB_data$genre5 == "Comedy" | RB_data$genre6 == "Comedy" | RB_data$genre7 == "Comedy")
RB_data$comedydummy[is.na(RB_data$comedydummy)] <- 0

RB_data$crimedummy <- as.numeric( RB_data$genre1 == "Crime" | RB_data$genre2 == "Crime" | RB_data$genre3 == "Crime" | RB_data$genre4 == "Crime" | RB_data$genre5 == "Crime" | RB_data$genre6 == "Crime" | RB_data$genre7 == "Crime")
RB_data$crimedummy[is.na(RB_data$crimedummy)] <- 0

RB_data$documentarydummy <- as.numeric( RB_data$genre1 == "Documentary" | RB_data$genre2 == "Documentary" | RB_data$genre3 == "Documentary" | RB_data$genre4 == "Documentary" | RB_data$genre5 == "Documentary" | RB_data$genre6 == "Documentary" | RB_data$genre7 == "Documentary")
RB_data$documentarydummy[is.na(RB_data$documentarydummy)] <- 0

RB_data$dramadummy <- as.numeric( RB_data$genre1 == "Drama" | RB_data$genre2 == "Drama" | RB_data$genre3 == "Drama" | RB_data$genre4 == "Drama" | RB_data$genre5 == "Drama" | RB_data$genre6 == "Drama" | RB_data$genre7 == "Drama")
RB_data$dramadummy[is.na(RB_data$dramadummy)] <- 0

RB_data$familydummy <- as.numeric( RB_data$genre1 == "Family" | RB_data$genre2 == "Family" | RB_data$genre3 == "Family" | RB_data$genre4 == "Family" | RB_data$genre5 == "Family" | RB_data$genre6 == "Family" | RB_data$genre7 == "Family")
RB_data$familydummy[is.na(RB_data$familydummy)] <- 0

RB_data$familydummy <- as.numeric( RB_data$genre1 == "Family" | RB_data$genre2 == "Family" | RB_data$genre3 == "Family" | RB_data$genre4 == "Family" | RB_data$genre5 == "Family" | RB_data$genre6 == "Family" | RB_data$genre7 == "Family")
RB_data$familydummy[is.na(RB_data$familydummy)] <- 0

RB_data$fantasydummy <- as.numeric( RB_data$genre1 == "Fantasy" | RB_data$genre2 == "Fantasy" | RB_data$genre3 == "Fantasy" | RB_data$genre4 == "Fantasy" | RB_data$genre5 == "Fantasy" | RB_data$genre6 == "Fantasy" | RB_data$genre7 == "Fantasy")
RB_data$fantasydummy[is.na(RB_data$fantasydummy)] <- 0

RB_data$historydummy <- as.numeric( RB_data$genre1 == "History" | RB_data$genre2 == "History" | RB_data$genre3 == "History" | RB_data$genre4 == "History" | RB_data$genre5 == "History" | RB_data$genre6 == "History" | RB_data$genre7 == "History")
RB_data$historydummy[is.na(RB_data$historydummy)] <- 0

RB_data$horrordummy <- as.numeric( RB_data$genre1 == "Horror" | RB_data$genre2 == "Horror" | RB_data$genre3 == "Horror" | RB_data$genre4 == "Horror" | RB_data$genre5 == "Horror" | RB_data$genre6 == "Horror" | RB_data$genre7 == "Horror")
RB_data$horrordummy[is.na(RB_data$horrordummy)] <- 0

RB_data$musicaldummy <- as.numeric( RB_data$genre1 == "Horror" | RB_data$genre2 == "Horror" | RB_data$genre3 == "Horror" | RB_data$genre4 == "Horror" | RB_data$genre5 == "Horror" | RB_data$genre6 == "Horror" | RB_data$genre7 == "Horror")
RB_data$musicaldummy[is.na(RB_data$musicaldummy)] <- 0


"Fantasy", "History", "Horror", "Musical", "Mystery", "News", "Romance", "Sci-Fi", "Sport", "Thriller", "War", "Western"

##To search for items within the Movie Title column
row <- grep(RB_data$movie_title,"The Dark Knight ")
as.character(RB_data$movie_title[1]) == as.character("The Dark Knight Rises ")
head(RB_data)
typeof(RB_data$genre1)
typeof(RB_data$movie_title)
RB_data$movie_title[1] == as.character("The Dark Knight Rises ")


#to search for a string in any column
str_detect(RB_data$movie_title[2],"John Carter")


##Establishing separate datasets for time periods


data11_15 <- filter(.data = RB_data, title_year < 2016)
#head(data11_15)
data16 <- filter(.data = RB_data,title_year == 2016)
#head(data16)
print(data11_15$movie_facebook_likes)