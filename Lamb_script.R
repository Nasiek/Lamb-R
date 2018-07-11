install.packages("readxl") # CRAN version
library("readxl")
install.packages("readr")
library(readr)
filename = "/Users/kl/Documents/NYCDA Bootcamp/Career/Lamb/vehiclesSplit.xlsx"


mysheets <- read_excel_allsheets("/Users/kl/Documents/NYCDA Bootcamp/Career/Lamb/vehiclesSplit.xlsx",col_guess())
list <- mysheets
#prints out the head of each element in the list. like wow just wow. so simple.
printList <- function(list) {
  
  for (item in 1:length(list)) {
    
    print(head(list[[item]]))
    
  }
}

#count the number of NA's in a dataframe by each column 
apply(list[[2]], 2, function(x) sum(is.na(x)))
# remove the NA's
list[[2]] <- list[[2]][complete.cases(list[[2]]), ]

#view head of first dataframe in list
head(list[[1]])
install.packages("stargazer")
library(stargazer)
#only does stats on columns whose rows are integers
stargazer(list[[1]], type ="text", title = "Summary Statistics")
# find unique rows count
unique(list[[1]]$make)
database <- read_excel("/Users/kl/Documents/NYCDA Bootcamp/Career/Lamb/vehiclesSplit.xlsx", sheet = 2)

View(database)
colnames(database)

install.packages("dplyr")
library(dplyr)


# Questions
# 1. What is the best model, for every make after Year 2000, 
#using the newest model and highway mpg as the benchmark. Remove unnecessary data

#duplicate list for this question
Q1 <- list
#check if dataset has any missing years
yearorder <- unique(list[[1]]$year)
sort(yearorder, decreasing = FALSE)
#subset dataframe by 2000 or greater
Q1vehicles <- Q1[[1]]
Q1vehicles2 <- Q1[[2]]

Q1vehicles[Q1vehicles$year>1999, ]

#time to merge dataframes to get highway values onto model and make values
Q1dataframe <- merge(Q1vehicles, Q1vehicles2, by="id")
#oops remove duplicated rows :)
install.packages("dplyr")
library(dplyr)
Q1dataframe <- Q1dataframe %>% distinct
#check length
nrow(Q1dataframe)
#check how many unique values are in the make and model columns and what are they?
#how many unique make
unique(Q1dataframe$make)
#how many unique model
unique(Q1dataframe$model) 
#too many. How many by each make


