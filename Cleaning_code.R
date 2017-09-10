setwd("/Users/sandhyasriraman/Downloads/")
moviedata = read.csv("main_movie_data1.1.csv", header = TRUE)
moviedata = na.omit(moviedata)
attach(moviedata)
summary(moviedata)

#removing unnecessary columns
moviedata$country = NULL
moviedata$genres = NULL
moviedata$language = NULL

#creating profit column
moviedata$profit = (gross / budget) - 1
#Data cleaning
#All data points are cleaned based on relative values to mean
moviedata=moviedata[moviedata$actor_1_facebook_likes>100,]
moviedata=moviedata[moviedata$actor_2_facebook_likes>100,]
moviedata=moviedata[moviedata$actor_3_facebook_likes>50,]
moviedata=moviedata[moviedata$director_facebook_likes>50,]

sd_gross=sd(moviedata$gross)


