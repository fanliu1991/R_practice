setwd("C:/Users/liufanl/Downloads")

library(tidyverse)

# in this file, library ggplot2 is used for data visualizations

# The general syntax of ggplot2 is:
# ggplot(dataset, aes(xaxis, yaxis)) +
#   geom_something() +
#   geom_somethingElse()

candyRankings <- read_csv("candy_data.csv")
candyProduction <- read_csv("candy_production.csv", col_types = "Dd")

# Scatter Plots
ggplot(data = candyRankings, aes(x = sugarpercent,
                                 y = pricepercent, 
                                 label = competitorname)) +
  geom_point() + 
  geom_smooth(method = "lm") + # add a fitted line
  geom_text(check_overlap = TRUE, # reduce overlap
            vjust = "bottom", # adjust the vertical orientation
            nudge_y = 0.01, # move the text up so it doesn't touch the points
            angle = 30,# tilt the text 30 degrees
            size = 2 # make the text smaller
  ) +
  labs(title = "More sugary candies are more expensive",
       x = "Sugar content (percentile)",
       y = "Price (percentile)"
  ) + 
  theme(plot.title = element_text(hjust = 0.5))
  

# extract canady features from column 2 to 10
candyFeatures <- candyRankings %>% select(2:10)
# convert numerical value 0 and 1 to boolean variable False and True
candyFeatures[] <- lapply(candyFeatures, as.logical)

# Bar Charts
ggplot(candyFeatures, aes(x = chocolate, fill = caramel)) + 
  geom_bar(position = "dodge") + # bars are located side-by-side, not stack
  facet_wrap(c("caramel")) + # put each level of "caramel" in a different facet
  scale_fill_manual(values=c("#BBBBBB", "#E69F00")) + 
  labs(title = "Chocolate candies are more likely to have caramel",
       x = "Is the candy chocolate?",
       y = "Count of candies") +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = c(0.9, 0.9), # move legend inside plot
        strip.background = element_blank(), # remove strip from top of facets
        strip.text.x = element_blank()) # remove text from top of facets


# Line Charts
ggplot(data = candyProduction, aes(x = observation_date, y = IPG3113N)) +
  geom_line() +
  geom_smooth(method = "loess") + 
  labs(title = "Monthly candy production (US)",
       x = "",
       y = "As percent of 2012 production") +
  theme(plot.title = element_text(hjust = 0.5))

# library(ggthemes)
# The ggthemes package has a lot of existing custom themes to be added
  
