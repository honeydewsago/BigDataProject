#load dplyr library
library(dplyr)
# glimpse(grocery) #structure of data

#implement library packages for parallelism functionality
library(parallel)
library(MASS)

# get the number of CPU cores
numCores <- detectCores() 
#get the number of physical cores
detectCores(logical = FALSE) 

grocery<-read.csv("C:/Users/Jia/Downloads/data.csv")

struct_grocery <- function(nstart) {
  class(grocery) #show data object class
  dim(grocery)   #show data dimension in total rows and columns
  names(grocery) #list all column names
  str(grocery)   #preview internal structure of data
  summary(grocery) #summary of data distribution
}

struct_grocery()

starts <- rep(1, 5) #100, 40
# fx <- function(nstart) kmeans(Boston, 4, nstart=nstart)
# numCores <- detectCores()
# numCores

#record the system time taken to process
system.time(
  results <- lapply(starts, struct_grocery)
)
