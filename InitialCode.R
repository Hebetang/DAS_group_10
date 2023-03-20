###Which properties of films influence whether they are rated by IMDB as greater than 7 or not?###

setwd("D:/Desktop/DAS group program/Group Project 2")
###0
#Preload all the packages
# install.packages('gapminder')
# install.packages('sjPlot')
# install.packages('stats')
# install.packages('jtools')
# install.packages('knitr')

library(tidyverse)
library(moderndive)
library(gapminder)
library(sjPlot)
library(stats)
library(jtools)
library(knitr)
library(ggplot2)
library(dplyr)

###1
##Read and merge the dataset
#Omit the NA values
film = read.csv("dataset10.csv")%>%
  na.omit()
#Create the binary factor
film$greater_than_7 <- ifelse(film$rating > 7, "yes", "no")
#Select dataset to omit the invalid column(eg.film_id and rating)
#Rename the dataset and transform the chr type value into factor
DataSet = film %>%
  dplyr::select(year, length, budget, votes, genre, greater_than_7)%>%
  mutate_if(is.character, as.factor)
#Set the level of response variable
levels(DataSet$greater_than_7) <- c("IMDB rate less than 7", "IMDB rate greater than 7")
#standardize the index

###2
##Plot to get an initial impression of the data
# Check correlations, distribution and print correlation coefficient 
ggpairs(data= film[,-1], title="Correlation between explanatory variables",ggplot2::aes(color=greater_than_7))

#Plot of year against greater_than_7
ggplot(data = DataSet, aes(x = greater_than_7, y = year, fill = greater_than_7)) +
  geom_boxplot() +
  labs(x = "IMDB rate greater than 7 or not", y = "Year", 
       title = "The boxplot of year and IMDB rate greater than 7 or not") +
  theme(legend.position = "none")

  
#Notice that the factor year seems not to influence rates
#Plot of length against greater_than_7
ggplot(data = DataSet, aes(x = greater_than_7, y = length, fill = greater_than_7)) +
  geom_boxplot() +
  labs(x = "IMDB rate greater than 7 or not", y = "Length",
       title = "The boxplot of length and IMDB rate greater than 7 or not") +
  theme(legend.position = "none")

#Plot of budget against greater_than_7
ggplot(data = DataSet, aes(x = greater_than_7, y = budget, fill = greater_than_7)) +
  geom_boxplot() +
  labs(x = "IMDB rate greater than 7 or not", y = "Budget",
       title = "The boxplot of budget and IMDB rate greater than 7 or not") +
  theme(legend.position = "none")

#Plot of votes against greater_than_7
ggplot(data = DataSet, aes(x = greater_than_7, y = votes, fill = greater_than_7)) +
  geom_boxplot() +
  labs(x = "IMDB rate greater than 7 or not", y = "Votes",
       title = "The boxplot of Votes and IMDB rate greater than 7 or not") +
  theme(legend.position = "none")

#Plot of genre against greater_than_7
ggplot(data = DataSet, aes(x= greater_than_7,  y = ..prop.., group=genre, fill=genre)) + 
  geom_bar(position="dodge", stat="count") +
  labs(x = "IMDB rate greater than 7 or not", y = "Proportion",
       title = "The Histograms of the proportion of movie categories and IMDB rate greater than 7 or not")



###3
##Modelling
model = glm(greater_than_7 ~ year + length + budget + votes + genre, data = DataSet, 
             family = binomial(link = "logit"))
model %>%
  summary()
###4
##Optimize the model
#Summarize the first model

#Raise the second model by the summary table
#Drop the year factorsince the p-value is large
model = glm(greater_than_7 ~ length + budget + votes + genre, data = DataSet, 
            family = binomial(link = "logit"))
#Summarize the second model
model %>%
  summary()

##5
#Plot the endpoints0 
#Plot log-odds
plot_model(model, show.values = TRUE, transform = NULL,
           title = "Log-Odds (IMDB rate greater than 7)", show.p = FALSE)
#Transform log-odds into odds
model %>%
  coef() %>%
  exp()
#Plot odds
plot_model(model, show.values = TRUE, 
           title = "Odds (IMDB rate greater than 7)", show.p = FALSE)
#Add probabilities to dataset
DataSet = DataSet %>%
  mutate(probs.greater_than_7 = round(fitted(model),4))%>%
  mutate(pred = ifelse(probs.greater_than_7>=0.5,"IMDB rate more than 7","IMDB rate less than 7"))

table = table(DataSet$greater_than_7,DataSet$pred)
(table[1,1]+table[2,2])/sum(table)





