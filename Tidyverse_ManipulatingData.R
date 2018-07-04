setwd("C:/Users/liufanl/Downloads")

library(tidyverse)

# in this file, library dplyr is used for manipulate data

farmData <- read.csv("farm_data.csv")

head(farmData, 10)
# get the number of rows and columns in dataset
dim(farmData)

# get a list of all column names in alphabetic order 
columnsAlphaOrder <- farmData %>% # take farmData and...
  names() %>% # get the column names and...
  sort() # sort alphabetically

# print the first six results
head(columnsAlphaOrder)


# Get Specific Columns With "select"
# select the column gender1, return the first six lines
farmData %>% 
  select("gender1") %>% 
  head()

# select every column except gender1
farmData %>% 
  select(-"gender1") %>% 
  head()

# select every column that starts with "gender"
farmData %>%
  select(starts_with("gender")) %>%
  head()

# select every column that don't starts with "gender"
farmData %>%
  select(-starts_with("gender")) %>%
  head()

# there are a number of special functions that only work inside select
# starts_with(), ends_with(), contains(), matches()
# num_range(), one_of(), everything()


# Get Specific Rows With "filter"
# return the rows where the vname is Tikare
farmData %>%
  filter(vname == "Tikare")

# return the rows where the vname is Tikare or Sefula
farmData %>% 
  filter(vname == "Tikare" | vname == "Sefula")

# return the rows where the number of farm plots is great than or equal to 6
farmData %>% 
  filter(fplots >= 6)

# return the rows where the village is Bonanza and there are less than three farm plots
farmData %>% 
  filter(vname == "Bonanza" & fplots < 3)


# Add New Variables With Mutate
# add a variable for whether this household owns land
farmData <- farmData %>%
  mutate(landowner = (tenure1 == 1 | tenure1 == 2)) 

summary(farmData$landowner)

# get the count of the number of men and women in each household, then add that to data frame.
# "1" is a man and "2" is a woman
# get the number of men for each household
countOfMen <- farmData %>% 
  select(starts_with("gender")) %>% # get only the columns with gender information
  magrittr::equals(1) %>% # look at the whole dataframe to see if each cell has "1" in it
  rowSums(na.rm = TRUE) # count the number of times when this is true, by row

# get the nubmber of women for each household
countOfWomen <- farmData %>% 
  select(starts_with("gender")) %>% 
  magrittr::equals(2) %>%
  rowSums(na.rm = TRUE)

# add these columns to dataframe
farmData <- farmData %>% 
  mutate(men = countOfMen, women = countOfWomen) # add new variables

# add a new column with the total number of men + women
farmData <- farmData %>%
  mutate(menPlusWomen = men + women)

# check the number of households that report household size different from
# the number of men & women who reported their gender
(farmData$hhsize == farmData$menPlusWomen) %>%
  na.omit() %>%
  table()


# Reorder Data With Arrange
# sort by the total number of men + women
farmData %>%
  select("X", "hhsize", "menPlusWomen") %>%
  arrange(desc(menPlusWomen)) %>% # sort in descending order
  head()

# look at the tail of sorted data instead of the head.
farmData %>%
  select(menPlusWomen, hhsize) %>%
  arrange(hhsize) %>% # sort by the hhsize value, descending
  tail()
# Arrange() will always put NA values at the end of dataframe


# Summarize variables with summarize
# get the mean household size, by two different estimations
farmData %>%
  summarize(meanMenPlusWomen = mean(menPlusWomen), #summerize the mean of menPlusWomen
            meanhhsize = mean(hhsize, na.rm = TRUE)) #mean of hhsize (removing NA's)

# number of rows without any information for gender and household size
farmData %>%
  summarize(missingMenPlusWomen = sum(menPlusWomen == 0),
            missinghhsize = sum(is.na(hhsize)))


# Analyze groups with group_by
# check if households that own land tend to have more plots to farm
farmData %>% 
  group_by(landowner) %>%
  summarize(plots = median(fplots, na.rm = TRUE))

# check the number of farms for each level of the plots
farmData %>% 
  group_by(fplots) %>%
  tally() # count the number of observations in each group

# count the count of each combination of "landowner" and "fplots"
farmData %>% 
  group_by(landowner, fplots) %>%
  tally() %>%
  na.omit()

# summarize which interviewers collected the most questionnaires
farmData %>% 
  group_by(interviewer) %>%
  tally() %>% # count the number of times each levels of the factor is observed
  filter(n > 50) %>%
  arrange(desc(n))
