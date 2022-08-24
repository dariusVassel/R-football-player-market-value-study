#Install and load packages

library(knitr)
install.packages("ggplot2")
library(ggplot2)
install.packages("dplyr")
library(dplyr)
install.packages("tidyr")
library(tidyr)
install.packages("stringr")
library(stringr)

#Introduction 
#With the increase in popularity of Soccer globally there is an in pour of money chasing the next best players. 
#Many a time players from a certain nationality, age and position command inflated market prices due to players 
#with a similar profile having great success in the past. With the help of data clubs can avoid overpaying for 
#such players and understand correlations of these factors in order to get a better understanding of the transfer 
#market. Data Analysis would help quantify the problem and back up decision making with concrete evidence. 
#I am trying to see if there is a correlation between soccer athletes salary and market value to factors such as 
#playing position, nationality and age. Researchers in the past too have aimed to gain a deeper understanding by 
#comparing skill and market value. While this is a great metric evaluating such a relation can be questionable as 
#greeing on a players skill as a number could be subjective. I believe hype and expectations for players of a certain 
#nationality and position impact the valuation of players since past performances of players from a certain region 
#inflate salaries. The study would provide insight into a valuation benchmark model that can help identify undervalued 
#players in the market for possible transfers and also give clubs an understanding of fair player wage compensation.

#Read Data
dataset <-read.csv('players_fifa22_formatted.csv',stringsAsFactors = TRUE)
str(dataset)
View(dataset)

#Cleaning & organizing data set

#Remove Null values in key variables - None Found
summary(dataset)

# Check for Dups - None Found
check_duplicates <-unique(dataset$ID)
nrow(dataset)
#View(check_duplicates)

# Check for missing values - None
Any_missing <- sum(is.na(dataset))
#View(Any_missing)



#Checking the degrees of freedom for the mahalanobis cutoff. 
#Summarizing mahal scores that are greater than the cutoff. 
#Checking the cut off score for our Mahalanobis measure.

mahal = mahalanobis(dataset[, 9:16], colMeans(dataset[, 9:16]), cov(dataset[, 9:16], use = "pairwise.complete.obs"))

cutoff = qchisq(1-0.001, ncol(dataset[, 9:16]))
cutoff

#Data Visulization
par(bg = "grey") #for plot() background graphics

# Seems to be a positive relationship between the two variables Overall and ValueEUR, so we should keep the variable in our dataset
test_2 <- lm(ValueEUR ~ Overall, data = dataset)
plot( dataset$ValueEUR ~ dataset$Overall, main="Overall Skill vs Market Value in 100 Millions of Euros", col = "white", xlab = "Overall Skill", ylab = "Market Value")
abline(test_2)

# Does not seems to be a direct relationship between the two variables height and ValueEUR, so we should remove the variable from our dataset
test_3 <- lm(ValueEUR ~ Height, data = dataset)
plot( dataset$ValueEUR ~ dataset$Height, main="Height vs Market Value in 100 Millions of Euros", col = "white", xlab = "Height", ylab = "Market Value")
abline(test_3)

# Does not seems to be a direct relationship between the two variables age and ValueEUR, so we should remove the variable from our dataset
test_4 <- lm(ValueEUR ~ Age, data = dataset)
plot( dataset$ValueEUR ~ dataset$Age, main="Age vs Market Value in 100 Millions of Euros", col = "white", xlab = "Age", ylab = "Market Value")
abline(test_4)

# Seems to be a positive relationship between the two variables WageEUR and ValueEUR, so we should keep the variable in our dataset
test_5 <- lm(ValueEUR ~ WageEUR, data = dataset)
plot( dataset$ValueEUR ~ dataset$WageEUR, main="Wage vs Market Value in 100 Millions of Euros", col = "white", xlab = "Age", ylab = "Market Value")
abline(test_5)

# Seems to be a positive relationship between the two variables Contract Until and ValueEUR, so we should keep the variable in our dataset
test_6 <- lm(ValueEUR ~ ContractUntil, data = dataset)
plot( dataset$ValueEUR ~ dataset$ContractUntil, main="Contract Until vs Market Value in 100 Millions of Euros", col = "white", xlab = "Age", ylab = "Market Value")
abline(test_6)

# Seems to be a positive relationship between the two variables Weak Foot Skill and ValueEUR, so we should keep the variable in our dataset
test_7 <- lm(ValueEUR ~ WeakFoot, data = dataset)
plot( dataset$ValueEUR ~ dataset$WeakFoot, main="Weak Foot Skill vs Market Value in 100 Millions of Euros", col = "white", xlab = "Age", ylab = "Market Value")
abline(test_7)

#Checking relationship between Left-Right Footed Players and ValueEUR
foot = lm(ValueEUR ~ PreferredFoot, data = dataset)
plot(dataset$ValueEUR ~ dataset$PreferredFoot)
ggplot(mutate(dataset, PreferredFoot = fct_infreq(PreferredFoot))) + geom_bar(aes(x = PreferredFoot))+cleanup


#OBSERVATION

#The graph (Overall x ValueEUR) above shows that the Overall of a player causes a huge impact on their market value. 
#Players with a overall of 70 are cheap but it is also evident that some better overall players have a lower market 
#value while having a similar skill level. Apart from a players overall skill level what   influences on players market price?
#  Some possible hypothesis to study are for why a player might have a lower market value despite a high skill level is-
  
#  HYPOTHESIS 1: Higher the players age makes a player a less attractive for a transfer if he only has a few years left despite being extremely skilled. 

#OBSERVATION 
#The graph (Age x ValueEUR) above shows that a majority of players see a decline in market value post 25 with a significant 
#decline occurring post 30.

#Market Value decline shows the risk appetite for clubs interested in older players lets see what relationship is observed in 
#overall skill level vs age.

#Looking at the correlation coefficient to quantify the strength of the relationship
cor(dataset$Overall, dataset$ValueEUR)

cor(dataset$ValueEUR, dataset$Age)

#Plotting the correlation of the ValueEUR score against the independent variables
corrplot::corrplot(cor(dataset[, 9:16]))

m2 = lm(ValueEUR ~ Overall, data = dataset)

summary(m2)


#Checking the highest influence for ValueEUR
summary(lm(ValueEUR ~ Overall, data = dataset)) #31%
summary(lm(ValueEUR ~ Height, data = dataset)) #0%
summary(lm(ValueEUR ~ Age, data = dataset)) #0.1%
summary(lm(ValueEUR ~ WageEUR, data = dataset)) #68%
summary(lm(ValueEUR ~ ContractUntil, data = dataset)) #4%
summary(lm(ValueEUR ~ WeakFoot, data = dataset)) #2%
summary(lm(ValueEUR ~ PreferredFoot, data = dataset)) #0%


#Determining the best model fit
best.model.fit = lm(ValueEUR ~ Overall + WageEUR + ContractUntil, data = dataset)
summary(best.model.fit)


#Determining the best model fit
best.model.fit_2 = lm(ValueEUR ~ Overall + WageEUR + ContractUntil + Height + Age + WeakFoot + PreferredFoot, data = dataset)
summary(best.model.fit_2)


{qqnorm(best.model.fit$residuals)
  qqline(best.model.fit$residuals)}

{qqnorm(best.model.fit_2$residuals)
  qqline(best.model.fit_2$residuals)}


