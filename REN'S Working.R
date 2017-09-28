library(dplyr)
library(readr)
library(tidyr)
library(magrittr)



data11_15 <- filter(.data = RB_data, title_year < 2016)
#head(data11_15)
data16 <- filter(.data = RB_data,title_year == 2016)
head(data16)

#All editing start here
#To measure popularity: consider variables such as- x=imdb score,y=return, 
library(ggplot2)
#scater plot
# scatter plot of facebook_likes vs return, 
  ggplot(data11_15, aes(x=movie_facebook_likes, y=return ))+ 
  geom_point()+
  geom_smooth()+
  ylim(0,8) 

##gross and return vs IMDB_SCORE-------anomoly is justin bieber, weak positive correlation
ggplot(data11_15, aes(x=imdb_score, y=gross ))+ 
  geom_point()+
  geom_smooth()
 
##scatter plot of imdb_score vs return, scale from 0-4, weak positive correlation
ggplot(data11_15, aes(x=imdb_score, y=return ))+ 
  geom_point()+
  geom_smooth()+
  ylim(0,4)

#sectter plot of imdb_score vs return, scale from 4-9, weak positive correlation
ggplot(data11_15, aes(x=imdb_score, y=return ))+ 
  geom_point()+
  geom_smooth()+
  ylim(4,9)

#imdb_score vs budget, relatively strong positive correlation
ggplot(data11_15, aes(x=imdb_score, y=budget))+  
  geom_point()+
  geom_smooth()+
  ylim(0,3e+08)
######
###



##################################################
#boxplot 
# boxplot of imdb vs return with return scale from 0 to 8 
geom_boxplot()+
ggplot(data11_15, aes(x=imdb_score, y=return ))+
  geom_smooth()+
  ylim(0,8)

# boxplot of imdb vs return with return scale from 4 to 8 
geom_boxplot()+
ggplot(data11_15, aes(x=imdb_score, y=return ))+
  geom_smooth()+
  ylim(4,8)

# boxplot of imdb vs gross
ggplot(data11_15, aes(x=imdb_score, y=gross ))+
  geom_boxplot()+
  geom_smooth()
##########
######


##Density plot  
#density plot showing the distribution of imdb_score, negatively skewed and most score centers on score 6 to 7  
ggplot(data11_15, aes(x=imdb_score))+  
  geom_density()

#density plot showing the distribution of facebook, centers on the left, can be excluded from further analysis.
ggplot(data11_15, aes(x=movie_facebook_likes))+  
  geom_density()
########
######



















