
readr("./Drafts/data_cleaning_draft2.R", "r")


#All editing start here
#To measure popularity: consider variables such as- x=imdb score,y=return, 
library(ggplot2)
#gross and return vs IMDB_SCORE-------anomoly
ggplot(RB_data, aes(x=imdb_score, y=gross ))+ ##gross and return vs IMDB_SCORE-------anomoly is justin bieber, weak positive correlation
  geom_point()+
  geom_smooth()
 
#
ggplot(RB_data, aes(x=imdb_score, y=return ))+ #scatter plot of imdb_score vs return, scale from 0-4, weak positive correlation
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
corvariables3 <- as.matrix(as.data.frame(corvariables1))
head(corvariables3)
cor(corvariables3)
cor(corvariables3$P)

#IMDB vs Revenue
ggplot(data11_15,aes=(x=movie_facebook_likes,y=imdb_score))+geom_point()

