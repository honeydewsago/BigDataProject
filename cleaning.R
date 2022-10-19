#install packages for data cleaning
install.packages("tidyverse")

#implement libraries
library(tidyverse)
library(dplyr)

grocery<-read.csv("C:/Users/Jia/Downloads/data.csv")
View(grocery)

# head(grocery)

#variable types
glimpse(grocery) #summary of dataset

class(grocery$month)
unique(grocery$month) #unique data in the column

names(grocery)

grocery %>% 
  dplyr::select(area_id, month, area) %>% 
  names()

unique(grocery$area)
