setwd("E:/R_exercise")

#library(tidyverse) # utility functions
library(rpart) # for regression trees
library(randomForest) # for random forests
library(modelr) # for mae function


melbourne_data <- read.csv("melb_data.csv")
head(melbourne_data)

# print a summary of the data in Melbourne data
summary(melbourne_data)


# train a decision tree based on melbourne_data dataset 
fit <- rpart(Price ~ Rooms + Bathroom + Landsize + BuildingArea +
               YearBuilt + Lattitude + Longtitude, data = melbourne_data)

# plot our regression tree 
plot(fit, uniform=TRUE)
# add text labels, make them 60% as big as they are by default
text(fit, cex=.6)

# the predictions of melbourne_data
predict(fit, head(melbourne_data))
# get the mean average error for decision tree model
mae(model = fit, data = melbourne_data)


# split melbourne_data to testing dataset and training dataset, 
# where testing = 30% and training = 70%
splitData <- resample_partition(melbourne_data, c(test = 0.3, train = 0.7))

# fit a new model to melbourne_data training set
fit2 <- rpart(Price ~ Rooms + Bathroom + Landsize + BuildingArea +
                YearBuilt + Lattitude + Longtitude, data = splitData$train)

# get the mean average error for our new model, based on our test data
mae(model = fit2, data = splitData$test)


# dropna drops missing values (think of na as "not available")
melbourne_data <- na.omit(melbourne_data)
splitData <- resample_partition(melbourne_data, c(test = 0.3, train = 0.7))

# train a decision tree based on melbourne_data dataset without missing value
fit <- rpart(Price ~ Rooms + Bathroom + Landsize + BuildingArea +
               YearBuilt + Lattitude + Longtitude, data = melbourne_data)


# a function to get the maximum average error for a given max depth
# predictors are passed as vector 
get_mae <- function(maxdepth, target, predictors, training_data, testing_data){
  
  # turn the predictors and target into a formula to pass to rpart()
  predictors <- paste(predictors, collapse="+")
  formula <- as.formula(paste(target,"~",predictors,sep = ""))
  
  # build model
  model <- rpart(formula, data = training_data,
                 control = rpart.control(maxdepth = maxdepth))
  # get the mae
  mae <- mae(model, testing_data)
  return(mae)
}

target <- "Price"
predictors <-  c("Rooms","Bathroom","Landsize","BuildingArea",
                 "YearBuilt","Lattitude","Longtitude")

# get the MAE for maxdepths between 1 & 10
for(i in 1:10){
  mae <- get_mae(maxdepth = i, target = target, predictors = predictors,
                 training_data = splitData$train, testing_data = splitData$test)
  print(glue::glue("Maxdepth: ",i,"\t MAE: ",mae))
}


# fit a random forest model to melbourne_data training set
fitRandomForest <- randomForest(Price ~ Rooms + Bathroom + Landsize + BuildingArea +
                                  YearBuilt + Lattitude + Longtitude, data = splitData$train)
# get the mean average error for random forest model, 
# based on melbourne_data test data
mae(model = fitRandomForest, data = splitData$test)

