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

old_data <- read.csv("./Drafts/awards_metadata.csv")
#head(old_data)
filter_data <- filter(old_data, Winner!="NA")
#head(filter_data)
select_data <- select(filter_data, Year, Award, Name, Film)
#head(select_data)
movie_wins <- filter(select_data, Award=="Best Picture" | Award=="Best Motion Picture")
#head(filter_data_movie_wins)
movie_wins <- count(movie_wins, Name)
head(movie_wins)
actor_wins <- filter(select_data, Award=="Actor" | Award=="Actress" | Award=="Actor in a Leading Role" | Award=="Actress in a Leading Role" | Award=="Actor in a Supporting Role" | Award=="Actress in a Supporting Role")
#head(filter_data_actor_wins)
actor_wins <- count(actor_wins, Name)
head(actor_wins)
director_wins <- filter(select_data, Award=="Directing")
director_wins$Name = director_wins$Film
director_wins <- count(director_wins, Name)
head(director_wins)
#typeof(director_wins)
total_wins <- rbind(actor_wins,director_wins,movie_wins)
head(total_wins)

#All editing start here
#To measure popularity: consider variables such as- x=imdb score,y=return, 
library(ggplot2)
#gross and return vs IMDB_SCORE-------anomoly
ggplot(RB_data, aes(x=imdb_score, y=gross ))+ ##gross and return vs IMDB_SCORE-------anomoly is justin bieber, weak positive correlation
  geom_point()+
  geom_smooth()
 
#
ggplot(RB_data, aes(x=imdb_score, y=return ))+ #scatter plot of imdb_score vs return, scale from 0-4, weak positive correlation
#gross and return vs IMDB_SCORE

#correlation test=0.339
imdb_vs_gross <- cor(RB_data$imdb_score,RB_data$gross)
print(imdb_vs_gross)
##gross and return vs IMDB_SCORE-------anomoly is justin bieber, weak positive correlation
ggplot(RB_data, aes(x=imdb_score, y=gross ))+ 
  geom_point()+
  geom_smooth()

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

#correlation test=-0.l08
imdb_score_vs_return <- cor(RB_data$imdb_score, RB_data$return)
print(imdb_score_vs_return)

#scatter plot of imdb_score vs return, scale from 0-4, weak positive correlation
ggplot(RB_data, aes(x=imdb_score, y=return ))+ 
  geom_point()+
  geom_smooth()+
  ylim(0,4)

ggplot(RB_data, aes(x=imdb_score, y=return ))+ #sectter plot of imdb_score vs return, scale from 4-9, weak positive correlation
  geom_point()+
  geom_smooth()+
  ylim(4,9)

#boxplot 
ggplot(RB_data, aes(x=imdb_score, y=return ))+
  geom_boxplot()
ylim(0,4)

ggplot(RB_data, aes(x=imdb_score, y=return ))+
  geom_boxplot()
ylim(4,8)

ggplot(RB_data, aes(x=imdb_score))+  #density plot showing the distribution of imdb_score, negatively skewed and most score centers on score 6 to 7
  geom_density()

ggplot(RB_data, aes(x=gross))+  #density plot showing the distribution of imdb_score, negatively skewed and most score centers on score 6 to 7
  geom_density()

ggplot(RB_data, aes(x=movie_facebook_likes))+ #density plot showing the distribution of imdb_score, negatively skewed and most score centers on score 6 to 7
  geom_density()

ggplot(RB_data, aes(x=return))+  #density plot showing the distribution of imdb_score, negatively skewed and most score centers on score 6 to 7
  geom_density()


ggplot(RB_data, aes(x=return))+  #density plot showing the distribution of imdb_score, negatively skewed and most score centers on score 6 to 7
  geom_density()


#correlation test=-0.0139
movie_facebook_likes_vs_return <- cor(RB_data$movie_facebook_likes, RB_data$return)
print(movie_facebook_likes_vs_return)

#correlation test=0.584
movie_facebook_likes_vs_gross <- cor(RB_data$movie_facebook_likes, RB_data$gross)
print(movie_facebook_likes_vs_gross)
# scatter plot of facebook_likes vs return
ggplot(RB_data, aes(x=movie_facebook_likes, y=return ))+  
  geom_point()+
  geom_smooth()+
  ylim(0,10)

ggplot(total_wins, aes(x=Name, y=n ))+ 
  geom_point()


readr("./Drafts/data_cleaning_draft2.R", "r")





#All editing start here

#To measure popularity: consider variables such as- x=imdb score,y=return, 

library(ggplot2)

#gross and return vs IMDB_SCORE-------anomoly

ggplot(RB_data, aes(x=imdb_score, y=gross ))+ ##gross and return vs IMDB_SCORE-------anomoly is justin bieber, weak positive correlation
  
  geom_point()+
  
  geom_smooth()


ggplot(RB_data, aes(x=budget, y=gross ))+ ##gross and return vs IMDB_SCORE-------anomoly is justin bieber, weak positive correlation
  
  geom_point()+
  
  geom_smooth()



#

ggplot(RB_data, aes(x=imdb_score, y=return ))+ #scatter plot of imdb_score vs return, scale from 0-4, weak positive correlation
  
  geom_point()+
  
  geom_smooth()+
  
  ylim(0,4)

ggplot(RB_data, aes(x=imdb_score, y=return ))+ #sectter plot of imdb_score vs return, scale from 4-9, weak positive correlation


ggplot(RB_data, aes(x=imdb_score, y=return ))+ #sectter plot of imdb_score vs return, scale from 4-9, weak positive correlation
  
  geom_point()+
  
  geom_smooth()+
  
  ylim(4,9)

#boxplot 
ggplot(RB_data, aes(x=imdb_score, y=return ))+
  geom_boxplot()
  ylim(0,4)

ggplot(RB_data, aes(x=imdb_score, y=return ))+
  geom_boxplot()
  ylim(4,8)
  
ggplot(RB_data, aes(x=imdb_score))+  #density plot showing the distribution of imdb_score, negatively skewed and most score centers on score 6 to 7
  geom_density()


#boxplot 

ggplot(RB_data, aes(x=imdb_score, y=return ))+
  geom_boxplot()





ggplot(RB_data, aes(x=imdb_score, y=return ))+
  
  geom_boxplot()+
  
  ylim(0,8)



ggplot(RB_data, aes(x=imdb_score))+  #density plot showing the distribution of imdb_score, negatively skewed and most score centers on score 6 to 7
  
  geom_density()





ggplot(RB_data, aes(x=movie_facebook_likes, y=return ))+ # ectter plot of facebook_likes vs return, 
  
  geom_point()+
  
  geom_smooth()+
  
  ylim(0,10)





##Correlation analysis





library(Hmisc)

corvariables1 <- data.frame(ret=data11_15$return, 
                            
                            fb=data11_15$movie_facebook_likes,
                            
                            imdb=data11_15$imdb_score,
                            
                            rev=data11_15$gross,
                            
                            bud=data11_15$budget)

corvariables3 <- as.matrix(as.data.frame(corvariables2))

head(corvariables3)

cor(corvariables3)

cor(corvariables3$P)



install.packages("corrplot")

library(corrplot)

corrplot(corvariables2, method ="circle")  

ggplot(RB_data, aes(x=movie_facebook_likes, y=return ))+ # ectter plot of facebook_likes vs return, 
  geom_point()+
  geom_smooth()+
  ylim(0,10)
#type="upper", order="hclust", tl.col="black", tl.srt=45, is.corr = FALSE)





str(corvariables2)

str(data11_15$movie_facebook_likes)

str(data11_15$gross)

str(data11_15$budget)

str(data11_15$return)

str(data11_15$imdb_score)



corrmatrix <- rcorr(data11_15$return, data11_15$movie_facebook_likes, data11_15$imdb_score, data11_15$gross, data11_15$budget)

corrmatrix





retfb <- cor.test(x = data11_15$return, y=data11_15$movie_facebook_likes)

retimdb <- cor.test(x = data11_15$return, y=data11_15$imdb_score)

retrev <- cor.test(x = data11_15$return, y=data11_15$gross)

retbud <- cor.test(x = data11_15$return, y=data11_15$budget)



fbimdb <- cor.test(x = data11_15$movie_facebook_likes, y=data11_15$imdb_score)

fbbud <- cor.test(x = data11_15$movie_facebook_likes, y=data11_15$budget)

fbrev <- cor.test(x = data11_15$movie_facebook_likes, y=data11_15$gross)



imdbrev <- cor.test(x = data11_15$imdb_score, y=data11_15$gross)

imdbbud <- cor.test(x = data11_15$imdb_score, y=data11_15$budget)



revbud <- cor.test(x = data11_15$gross, y=data11_15$budget)



fbrev

imdbrev

plot(x= data11_15$gross, y=data11_15$movie_facebook_likes)

plot(x= data11_15$return, y=data11_15$movie_facebook_likes)


plot(x= data11_15$gross, y=data11_15$imdb_score)

plot(x= data11_15$return, y=data11_15$movie_facebook_likes)

m1 <- lm(gross~budget+imdb_score+movie_facebook_likes+num_voted_users+num_user_for_reviews,data11_15)
summary(m1)#budget, num_voted_users and so on are determinants, exclude facebook likes




