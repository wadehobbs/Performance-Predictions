library(tidyverse)
library(summarytools) #Summarise data
library(rpart) #Build decision trees
library(rattle) #Viz for decision tree
library(caret) #Confusion matrix
#Data
#https://www.kaggle.com/heesoo37/120-years-of-olympic-history-athletes-and-results

#Import data
hist_oly_db <- read_csv("~/Dropbox/Personal/Jobs/Performance Predictions/120-years-of-olympic-history-athletes-and-results-1/athlete_events.csv")

#data summary
view(dfSummary(hist_oly_db))

#Shows the main outcome variable (medal) is mostly missing data. Will need to change NAs to something useful
hist_oly_db$Medal[is.na(hist_oly_db$Medal)]<-"No_Medal"

#Will add a classifier column for medal. Now have ordinal (Medal col) and categorical data (Medal_Classification col)
#Will need to supress one or the other depending on the specified outcome variable.
hist_oly_db$Medal_Classification <- ifelse(hist_oly_db$Medal == 'No_Medal', 'No_Medal', 'Medal')
#Subset of possibly predictive variables
oly_subset <- select(hist_oly_db, c(.data$Sex, .data$Age, .data$Height, .data$Weight, .data$NOC, .data$Year, .data$Sport, .data$Event, .data$Medal_Classification))

#Going to start with a decision tree to predict medal or no medal.
#Split data into train and test set
train <- sample_frac(oly_subset, 0.8)
test <- sample_frac(oly_subset, 0.2)

#Defining model. Method class tells rpart to make a classification model.
#Data argurment for the model is all the data minus the Medal col as this will be highly predictive of the outcome, since it was used to make the column
medal_model <- rpart(Medal_Classification ~ ., train, method = 'class', control = rpart.control(minsplit = 20, cp = 0.01))
medal_predict <- predict(object = medal_model, newdata = test, type = 'class')
confusionMatrix(data = medal_predict, reference = factor(test$Medal_Classification))
#Results show that the classifier stinks. Though this is not surprising given the predictors. 
#Its great at predicting No_Medal (99.4%) but terrible at predicting a medal (12%). 
#Either it needs better tuning or the predictors are just bad. 

#Plot doesnt really work either. Ends up having huge amounts of text under the branches. 
fancyRpartPlot(medal_model)

