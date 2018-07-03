# R_practice

%>%
This symbol is called a "pipe" and is used to "pipe" the output of one function into another function.

for example:

c(1,2,5,5,10) %>%      # concatenate these five numbers and then...
    mean() %>%         # get the mean and then...
    prod(5)            # multiply by five

# this does the same thing as the above code, but is much harder to read (and debug!)
prod(mean(c(1,2,5,5,10)),5)

Notes:
1. 
pipes do not change the things you put into the pipes

e.g.
# save our vector to a test variable
myVariable <- "some text"

# do some piped operations
myVariable %>%
  strsplit(split = "") %>% # split into characters
  unlist() %>% # convert list to vector
  sort() # alphabatize
[1] " " "e" "e" "m" "o" "s" "t" "t" "x"

# the things put into piped operations
myVariable
[1] "some text"

2.
pipes will pass the output of whatever you're piping in as the first argument to the next function.
e.g.
# this will round the first number to two dicimal places,
piped <- 3.14152 %>%
    round(1.5690)
    
# same as
round(3.14152, 1.5690)
