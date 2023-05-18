# Lecture 2b. Data Exploration and Management
# Missing Values
NA > 5 
10 == NA 
NA == NA 
is.na(c(1, NA, 3)) 

# scale() function
mydata <- data.frame("seq"=c(1,2,3,4,5),"count"=c(2,4,7,12,34))
mydata2 <-scale(mydata)
sum(mydata2[,1])
sd(mydata2[,1])

# data selection
#iris data comes pre-installed with R and is part of the "datasets" package
summary(iris)  
head(iris)
columns_we_want <- c("Petal.Length", "Petal.Width", "Species")
rows_we_want <- iris$Petal.Length > 2
iris_base <- iris[rows_we_want, columns_we_want, drop = FALSE]
head(iris_base) # display the beginning of the data set
tail(iris_base) # display the end of the data set

# Remove observations
if(!require("ggplot2")) {
  install.packages("ggplot2")
  library('ggplot2')      # Load ggplot2 package
}
 
data(msleep)            # load msleep data from ggplot2 package
str(msleep)
summary(msleep)

# use complete.cases() to select rows without missing values
clean_base_1 <- msleep[complete.cases(msleep), , drop = FALSE]
summary(clean_base_1)
nrow(clean_base_1)
# use na.omit() to select rows without missing values
clean_base_2 = na.omit(msleep)
nrow(clean_base_2)

# use order() to order rows
purchases <- data.frame("day"=c(1,2,2,1,2,1),"hour"=c(9,9,11,13,13,14),"n_purchase"=c(5,3,5,1,3,1))
purchases
order_ind<-order(purchases$day, purchases$n_purchase)
order_ind
purchases[order_ind,,drop=FALSE]

# Separating ordered rows into groups and run calculations on each group
order_index <- with(purchases, order(day, hour))
# The base function with() executes the code in its second argument as if the columns of the first argument were variables.
purchases_ordered <- purchases[order_index, , drop = FALSE]
data_list <- split(purchases_ordered, purchases_ordered$day)
#The base function split(x,f) divides the data in x into the groups defined by f, returns a list. 
data_list
data_list <- lapply( 
  data_list,
  function(di) {
    di$running_total <- cumsum(di$n_purchase)
    di
  })
# The base function lapply(X,FUN) returns a list of the same length as X, each element of which is the result of applying FUN to the corresponding element of X. 
data_list
purchases_ordered <- do.call(base::rbind, data_list) 
# The base function do.call() constructs and executes a function call from a name or a function and a list of arguments to be passed to it.
# or purchases_ordered <- rbind(data_list[[1]],data_list[[2]])
purchases_ordered

# (i) Basic data transformation

# adding or deleting a column
library('datasets')
head(airquality)    #“airquality" is a data set in package ‘datasets’
myaqdata <- airquality
# add a new column of data
myaqdata$newcol <- paste(myaqdata$Month,myaqdata$Day,sep="-")
head(myaqdata)
myaqdata$newcol <- NULL        # delete a column
head(myaqdata)
tail(myaqdata)               # display the end of the data set

# Changing columns and selecting rows using transform() and subset() functions
myaqdata <- transform(myaqdata,newcol=paste(myaqdata$Month,myaqdata$Day,sep="."))
head(myaqdata)
#transform(`mydata`, ...): ... are tagged vector expressions, which are evaluated in 
#the data frame mydata. The tags are matched against names(mydata), and for those that 
#match, the value replace the corresponding variable in mydata, and the others are 
#appended to mydata
myaqdata <- subset(airquality,!is.na(Ozone), select=c("Ozone","Month"))
head(myaqdata)
# subset(x, argu, select) returns a subset of x. argu selects rows, select selects columns. 

# Replacing a NA with the most recent non-NA prior to it.
# Use the na.locf() function from the package "zoo"
if(!require("zoo")){
  install.packages("zoo")
  library("zoo")
}
myaqdata <- airquality
head(myaqdata)
myaqdata$Ozone <-na.locf(myaqdata$Ozone, na.rm=FALSE)
# always use na.rm = FALSE with na.locf(), otherwise it may delete the initial NA values from your data
head(myaqdata)

# (ii) Aggregating transformation
# Combining multiple rows or columns
# (1) Creating a vector of data with no missing values from the first non-missing 
# values of multiple vectors (known as coalesce in SQL) using wrapr::coalesce(left, right)
# or simply left %?% right (Note: %?% is the infixed operator for coalesce)
data <- data.frame("time"=c(1L,2L,3L,4L),"s1"=c(NA,NA,NA,NA),"s2"=c(-0.7,0.2,NA,NA), "s3"=c(0.8, NA,0.9, NA))
data
if(!require("wrapr")) {
  install.packages("wrapr")
  library("wrapr")
}
data$complete<-data$s1 %?% data$s2 %?% data$s3 %?% 0.0
data

#(2) Combining multiple rows into summary rows using base functions such as tapply(), 
# aggregate(), tabulate(), table(). (not very convenient)
purchases <- data.frame("day"=c(1,2,2,1,2,1),"hour"=c(9,9,11,13,13,14),"n_purchase"=c(5,3,5,1,3,1))
purchases

x1 <- tapply(purchases$hour,purchases$day,sum)
# tapply(X, INDEX, FUN) applies function FUN on X (a vector-like R object) that are 
# grouped based on INDEX.
x1
x2<-tapply(purchases$n_purchase,purchases$day,sum)
x2
dayno<-factor(purchases$day)
sumpur<-cbind(day=as.numeric(levels(dayno)),sum_hour=x1,sum_n_purchase=x2)
sumpur

aggregate(purchases, list(purchases$day), sum)[,c(1,3,4)]
# aggregate() splits the data into subsets, computes summary statistics for each,
# and returns the result in a convenient form.

tabulate(c(-2,0,2,3,3,5), nbins = 3)
# tabulate(bin,nbins) takes the integer-valued vector bin and counts the frequencies 
# of integers of 1, 2, ..nbins.
tabulate(c(2,3,3,5), nbins = 10)

with(airquality, table(cut(Temp, quantile(Temp)), Month))
# table() creates a contingency table

# (iii). Multi-Table Data Transform
# Split a data frame into multiple data frames by split(x,f) function: 
# it divides the data in x into the groups defined by f.
temp <- split(purchases,purchases$day)
temp
# Split a data frame into multiple data frames by using logical vector as index of rows
purchases1 <- purchases[purchases$day==1,]
purchases2 <- purchases[purchases$day==2,]

# Combining multiple data frames by rows using the base R function rbind()
purchases <-rbind(purchases1, purchases2)
purchases
# Combining multiple data frames by columns using the base R function cbind()
temp <- cbind(purchases1, purchases2)
temp
# Note: it is not a good idea to have multiple columns to have the same name
if(!require("janitor")) {
  install.packages("janitor")
  library("janitor")
  }
janitor::clean_names(temp)
# The janitor Package in R can be used to clean and examine data

# Joining tables by matching rows based on key values
# Left Join: keeps row from the left table and adds columns from matching rows in the right table.
# Using base R function merge() with argument all.x=TRUE. 
# Note that merge() merges two data frames by common columns or row names, 
#   or do other versions of database join operations.

capacityTable <- data.frame("RoadID"=c("r1","r3","r4","r5"),"capacity"=c(9.99,19.99,5.49,24.49))
demandTable <- data.frame("RoadID"=c("r1","r2","r3","r4"),"capacity"=c(10,43,55,8))
capacityTable
demandTable
merge(capacityTable,demandTable, by ="RoadID",all.x=TRUE)

# Right Join: keeps row from the right table and adds columns from matching rows in the left table.
# Using base R function merge() with argument all.y=TRUE. 
merge(capacityTable,demandTable, by ="RoadID",all.y=TRUE)

# Inner Join: keeps rows where the key exists in both tables. This produces an intersection of two tables.
# Using base R function merge()
merge(capacityTable,demandTable, by ="RoadID")

# Full Join: keeps rows for all key vales. Notice the two tables have equal importance here.
# Using base R function merge()
merge(capacityTable,demandTable, by ="RoadID", all=TRUE)

#3.3 Data Reshaping
# Use tidyverse or data.table packages, or other packages










 

