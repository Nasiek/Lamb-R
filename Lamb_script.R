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
#remove duplicated rows :)
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
#too many. Next, split dataset for each make and put into list
#duplicate original dataframe
Q1_duplicate <- Q1dataframe
make_list <- split(Q1_duplicate, Q1_duplicate$make)
#duplicate list
make_list_duplicate <- make_list
#find the rows in the top 95% of the year column of each dataframe
#x <- rnorm(100)
#quantile(x, probs = c(0, 0.25, 0.5, 0.75, 0.95,1))
#y <- subset(x, Code==1)
#break data into quantile for highest years
head(make_list_duplicate[[1]])
## trying this with the whole dataframe instead of the list
df_list <- split(Q1dataframe, as.factor(Q1dataframe$year))#this just turns it into a list again
#for loop
install.packages('Hmisc')
library(Hmisc)
uniq <- unique(unlist(Q1dataframe$make))
class(uniq)
for (i in 1:length(uniq)){
  data_1[[i]] <- subset(Q1dataframe, make == uniq[i]);
#  data_1[i] <- data_1[i][lapply(data_1[i],length)>0]
  data_1[[i]] <- sapply(data_1[i], function(x) length(unique(data_1[[i]])) >3);
  ##cut2!!
  data_1[[i]]$yr_quantile <- cut2(as.matrix(data_1[[i]]$yr), 
                                1, 4, TRUE, minmax=TRUE, oneval=TRUE, 
                                onlycuts=TRUE);
  data_1[[i]]$hwy_quantile <- cut2(as.matrix(data_1[[i]]$hwy), 
                                 1, 4, TRUE, minmax=TRUE, oneval=TRUE, 
                                 onlycuts=TRUE);
  ##cut2 ends
  ifelse (data_1[[i]]$yr_quantile == 4 & 
            data_1[[i]]$hwy_quantile == 4, 
          best_make <-rbind(data_1[[i]]
          [which.max(data_1[[i]]$hwy),],0),best_make); 
  
  #your desired function
}
##original cuts
data_1[i]$yr_quantile<- cut(as.matrix(data_1[i]$year), 
                            breaks=unique(quantile(data_1[i]$year),
                                          labels=1:4, include.lowest=TRUE));
data_1[i]$hwy_quantile <- cut(as.matrix(data_1[i]$hwy), 
                              breaks=unique(quantile(data_1[i]$hwy),
                                            labels=1:4, include.lowest=TRUE));
##
##cut2!!swap out
data_1[i]$yr_quantile <- cut2(as.matrix(data_1[i]$yr), 
                              1, 4, TRUE, minmax=TRUE, oneval=TRUE, 
                              onlycuts=TRUE);
data_1[i]$hwy_quantile <- cut2(as.matrix(data_1[i]$hwy), 
                               1, 4, TRUE, minmax=TRUE, oneval=TRUE, 
                               onlycuts=TRUE);
##cut2 ends


#___________________
#do this all in a forloop
#for(i in dataList){for(j in i){print(j)}}
options(error=recover)

for(i in make_list){
  make_list[[i]]['yr_quantile'] <- cut(as.data.frame(make_list)[[i]]['year'] , breaks=quantile(make_list)[[i]]['year'],
    labels=1:4, include.lowest=TRUE);
  make_list[[i]]['hwy_quantile'] <- cut(as.data.frame(make_list)[[i]]['hwy'] , breaks=quantile(make_list)[[i]]['hwy'],
  labels=1:4, include.lowest=TRUE);
  ifelse (make_list[[i]]['yr_quantiles'] == 4 & 
  make_list[[i]]['hwy_quantile'] == 4, best_car_model <- 
    rbind(make_list[[i]][which.max(make_list[[i]]['hwy']),],0),best_car_model) 
}

##sample code to select columsn from a list
myarray <- unlist(mylist, use.names = FALSE)
dim(myarray) <- c(nrow(mylist$a), ncol(mylist$a), length(mylist))
dimnames(myarray) <- list(hour = rownames(mylist$a),
                           week = colnames(mylist$a),
                           other = names(mylist)) # now you can do:
mean(myarray[, "week1", "a"])
# or:
colMeans(myarray)


listyear<- lapply(make_list, `[`, `year`)
  for(j in i)
#{print(head(j)}  
#}
#make_list$[[1:length(make_list)]] <- lapply(make_list, `[`, 'year')
make_list2 <- make_list
listYearCols <- lapply(make_list2, `[`, 'year')
View(listYearCols[[1]])
listYearColQuants <- lapply(listYearCols,cut(as.numeric(listYearCols) , breaks=quantile(as.numeric(listYearCols)),
    labels=1:4, include.lowest=TRUE))


yearQuantsList <-lapply(listYearCols, function(listYearCols) 
      cbind(make_list2, yr_quantile = listYearCols))    
    
make_list <- mapply(cbind, make_list, "yr_quantile"=ID, SIMPLIFY=F)    
    
year.column <- lapply(make_list, cut(listYearCols , breaks=quantile(listYearCols),
                                                      labels=1:4, include.lowest=TRUE))
second.step <- lapply(first.step, next.function)
The 





eg1 <- make_list_duplicate[[1]]
eg1$quantiles <- cut(eg1$year , breaks=quantile(eg1$year),
             labels=1:4, include.lowest=TRUE)
#check the range cut offs
tapply(eg1$year , eg1$quantiles , range)
#and the quantile values themselves
quantile(eg1$year)
#Now do the same thing for the hwy miles per gallon values
#or just subset the top years and subset that dataframe to then find the top mpg
eg1_topyr <- eg1[eg1$quantiles == 4,] #to see the highest hwympg sort by hwy^
#find highest hwy mpg figures
#first get quantiles
eg1_topyr$hwy_quantiles <- cut(eg1_topyr$year , breaks=quantile(eg1_topyr$year),
                     labels=1:4, include.lowest=TRUE)
#then get the values themselves
quantile(eg1_topyr$year)
#if both hwy quantile value and quantile value = 4, get highest hwy value
ifelse (eg1_topyr$quantiles == 4 & eg1_topyr$hwy_quantiles == 4, acura_max <-rbind(eg1_topyr[which.max(eg1_topyr$hwy),],0),acura_max) 
#merge to colMax dataframe when doing this with a new row
#put above function inside an Rbind as an argument?
acura_max <- rbind(colMax,acura_max)
{
  colMax <- eg1_topyr[which.max(eg1_topyr$hwy),] }

#na.omit([gifts < Date_3])



