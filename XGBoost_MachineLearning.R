setwd("C:/Users/liufanl/Downloads")

library(xgboost)
library(tidyverse)
library(DiagrammeR)

# dataset from the Food and Agriculture Organization of the United Nations,
# contains information on various outbreaks of animal diseases
diseaseInfo <- read_csv("Outbreak_240817.csv")

# shuffle dataset
set.seed(2333)
diseaseInfo <- diseaseInfo[sample(1:nrow(diseaseInfo)), ]

# remove the columns that have information related to target variables
diseaseInfo_sicknessRemoved <- diseaseInfo %>%
  select(-starts_with("human"))

# create a boolean vector indicating whether humans are affected
diseaseLabels <- diseaseInfo %>%
  select(humansAffected) %>% 
  is.na() %>% 
  magrittr::not() # switch TRUE and FALSE

# remove redundant information and select numeric columns only
diseaseInfo_numeric <- diseaseInfo_sicknessRemoved %>%
  select(-Id) %>%
  select(-c(longitude, latitude)) %>%
  select_if(is.numeric)

# add a boolean column to numeric dataframe indicating whether a species is domestic
diseaseInfo_numeric$is_domestic <- str_detect(diseaseInfo$speciesDescription, "domestic")

# get a list of all the species by getting the last word in speciesDescription
speciesList <- diseaseInfo$speciesDescription %>%
  str_replace("[[:punct:]]", "") %>% # remove punctuation (some rows have parentheses)
  str_extract("[a-z]*$") # extract the least word in each row

# convert species list into a dataframe
speciesList <- tibble(species = speciesList)
# convert to a matrix using one-hot encoding
options(na.action='na.pass') # not drop NA values
species <- model.matrix(~species-1, speciesList)

# convert country to numeric variables using one-hot encoding
region <- model.matrix(~country-1, diseaseInfo,)


# combine numerical dataframes and convert them into a matrix
diseaseInfo_numeric <- cbind(diseaseInfo_numeric, region, species)
diseaseInfo_matrix <- data.matrix(diseaseInfo_numeric)


# split dataset into testing and training subsets
split_at <- round(length(diseaseLabels) * 0.7)
# training data
train_data <- diseaseInfo_matrix[1:split_at, ]
train_labels <- diseaseLabels[1:split_at]

# testing data
test_data <- diseaseInfo_matrix[-(1:split_at),]
test_labels <- diseaseLabels[-(1:split_at)]


# train a model
model <- xgboost(data=train_data, label=train_labels, nround=2, objective="binary:logistic")
# generate predictions for testing data
prediction <- predict(model, test_data)
# get the classification error
error <- mean(as.numeric(prediction > 0.5) != test_labels)


# parameter tuning on:
# number of layer in decision tree,
# number of training rounds,
# early_stopping_rounds, if there is no improvement in this many rounds, stop
# scale_pos_weight, account for imbalanced classes
# gamma: a regularization term, a proposed model is included if it reduces loss by at least gamma value

negative_cases <- sum(train_labels == FALSE)
postive_cases <- sum(train_labels == TRUE)

model_tuned <- xgboost(data=train_data, label=train_labels, 
                       max.depth = 3, 
                       nround = 10, 
                       early_stopping_rounds = 3,
                       objective="binary:logistic", # the objective function 
                       scale_pos_weight = negative_cases/postive_cases, # control for imbalanced classes
                       gamma = 1)
# generate predictions and classification error
prediction_tuned <- predict(model_tuned, test_data)
error_tuned <- mean(as.numeric(prediction_tuned > 0.5) != test_labels)

# a representation of the combination of all the decision trees in the model
# stack them all on top of one another and pick the things that show up most often in each node
xgb.plot.multi.trees(feature_names = names(diseaseInfo_matrix), model = model_tuned)

# since a logistic model is used here, the log-odds is shown rather than the probability
# convert log odds to probability
odds_to_probs <- function(odds){
  return(exp(odds)/ (1 + exp(odds)))

odds_to_probs(-0.59857)

# get information on how important each feature is and plot it
importance_matrix <- xgb.importance(names(diseaseInfo_matrix), model = model_tuned)
xgb.plot.importance(importance_matrix)

