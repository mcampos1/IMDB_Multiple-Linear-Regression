#Martin Campos 
#INST 314 Project 4 
#Revised Date: 5/14/18
install.packages("stargazer")
install.packages("car")
library(summarytools)
library(stargazer)
library(car)
#load data and subset data
movies <- read.csv(file.choose())
usa <- subset(movies, movies$country == "USA")
bud <- usa$budget
a1fb <- usa$actor_1_facebook_likes
a2fb <- usa$actor_2_facebook_likes
dfb <-  usa$director_facebook_likes
imdb <- usa$imdb_score
#summary statistics
descr(bud)#primary actor FB likes
descr(a1fb)
descr(a2fb)
descr(dfb)
descr(imdb)
freq(a1fb)
freq(a2fb)
freq(dfb)
freq(bud)
freq(imdb)
options(scipen = 5)
plot(bud+a1fb+a2fb+dfb,imdb,
     col="red",
     main = "IMDB Score vs Budget, Facebook likes of Primary Actor, Secondary Actor and Director",
     xlab = "Budget and Facebook likes of Actor1, Actor 2, and, Director",
     ylab = "IMDB Score"
     )
par(mfrow=c(2,2))
plot(lm(imdb ~ a1fb+a2fb+dfb+bud,data = movies))
abline(lm(imdb ~ a1fb+a2fb+dfb+bud,data = movies))
par(mfrow=c(1,1))
summary(OLS)
OLS <- lm(formula = imdb ~ a1fb+a2fb+dfb+bud,data = movies)
setwd("C:/Users/martincampos/Documents")
stargazer(OLS, type = "html", out = "PR4.html", dep.var.labels ="IMDB Score", 
          covariate.labels = c("Primary Actor Facebook Likes", "Secondary or other Actor Facebook Likes", "Director Facebook Likes", "Movie Budget"))


