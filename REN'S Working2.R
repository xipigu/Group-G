
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
corvariables3 <- as.matrix(as.data.frame(corvariables2))
head(corvariables3)
cor(corvariables3)
cor(corvariables3$P)

install.packages("corrplot")
library(corrplot)
corrplot(corvariables2, method ="circle")  
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







