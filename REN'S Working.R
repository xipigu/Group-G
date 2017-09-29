library(dplyr)
library(readr)
library(tidyr)
library(magrittr)

old_data <- read.csv("C:/Users/Ryan/Desktop/movie_metadata.csv")
#head(old_data)
filter_data <- filter(old_data, country=="USA", language=="English")
#head(filter_data)
select_data <- select(filter_data, movie_title, title_year, director_name, duration, gross, genres, num_voted_users, num_user_for_reviews, budget, imdb_score, movie_facebook_likes, actor_1_name, actor_2_name, actor_3_name)
#head(select_data)
select_data_filter <- filter(select_data, title_year >= 2011, gross !=0 | gross != "NA" | gross != "", imdb_score != 0 | imdb_score != "NA" | imdb_score != "",budget != 0 | budget != "NA" | budget != "", movie_facebook_likes != 0 | movie_facebook_likes != "NA" | movie_facebook_likes != "")
#head(select_data_filter)

##Genre conversion

typeof(select_data_filter$genres)
toString(select_data_filter$genres)
str(select_data_filter)
genre_names <- c("Action","Adventure", "Animation", "Biography", "Comedy", "Crime", "Documentary", "Drama", "Family", "Fantasy", "History", "Horror", "Musical", "Mystery", "News", "Romance", "Sci-Fi", "Sport", "Thriller", "War", "Western")
genre_types <- c("genre1", "genre2","genre3", "genre4", "genre5", "genre6", "genre7")

genre_filter <- select_data_filter %>% separate(genres, into=genre_types,sep = "\\|")
#head(genre_filter)

##Establishing a Gross Revenue / Budget variable

RB_data <- mutate(.data = genre_filter, return = gross / budget)
RB_data$return <- round(RB_data$return, 2)
#head(RB_data)

##Establishing separate datasets for time periods

data11_15 <- filter(.data = RB_data, title_year < 2016)
#head(data11_15)
data16 <- filter(.data = RB_data,title_year == 2016)
head(data16)

#All editing start here
#To measure popularity: consider variables such as- x=imdb score,y=return, 
library(ggplot2)
#IMDB Rating vs Revenue
ggplot(RB_data, aes(x=imdb_score, y=gross ))+
  geom_point()+
  geom_smooth()
 
#IMDB Rating vs Return

ggplot(RB_data, aes(x=imdb_score, y=return ))+ 
  geom_point()+
  geom_smooth()+
  ylim(0,10)

#Facebook likes vs Returns
ggplot(RB_data, aes(x=movie_facebook_likes, y=return ))+ 
  geom_point()+
  geom_smooth()+
  ylim(0,10)

#Facebook likes vs Revenues
ggplot(RB_data, aes(x=movie_facebook_likes, y=gross ))+ 
  geom_point()+
  geom_smooth()

ggplot(RB_data, aes(x=movie_facebook_likes, y=imdb_score ))+ 
  geom_point()+
  geom_smooth()

ggplot(RB_data, aes(x=movie_facebook_likes, y=num_voted_users))+ 
  geom_point()+
  geom_smooth()

ggplot(RB_data, aes(x=movie_facebook_likes, y=num_user_for_reviews))+ 
  geom_point()+
  geom_smooth()
# 1.Budget vs IMDB rating

ggplot(RB_data, aes(x=imdb_score, y= budget)) +geom_point()+geom_smooth()+ylim(90000,263700000)

# 1.budget vs revenue
ggplot(RB_data, aes(x=budget,y=revenue))+geom_point()+geom_smooth()

  
  
# 2. Directors vs IMDB rating and rating


Counting_directors <- for (i in RB_data$director_name){(RB_data$imdb_score)}



# 4.Award vs IMDB rating
ggplot(RB_data, aes(x=award, y= imdb_score)) +geom_point()+geom_smooth()

#4. Award vs revenue rating
ggplot(RB_data, aes(x= award, y=gross)) + geom_point()+geom_smooth()


