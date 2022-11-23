#install required packages
install.packages("parallel")
install.packages("microbenchmark")
install.packages("dplyr")
install.packages("tidyverse")
install.packages("data.table")

#load packages
library(parallel)
library(microbenchmark)

#detect available physical and logical processors cores
numCores <- detectCores() 
detectCores(logical = FALSE)

#set working directory
setwd("C:/Sem6/5011CEM BigData/BigDataProject/datasets/")

#function to read a list of dataset files
library(dplyr)
library(tidyverse)
library(data.table)

df <- function(i){
  list.files(path = "C:/Sem6/5011CEM BigData/BigDataProject/datasets/", pattern = "*.csv") %>%
  map_df(~fread(.))
}
df

#sequential processing
seq <- lapply(1:50, df)

#parallel processing using sockets
#create clusters for each cores
cl <- makeCluster(numCores)
#add libraries to each clusters
clusterEvalQ(cl, {
  library(dplyr)
  library(tidyverse)
  library(data.table)
  library(lme4)
})
#run parallel processing
par <- parLapply(cl, 1:50, df)
#close cluster
stopCluster(cl)

#compare both functions performance using microbenchmark
mbm <- microbenchmark("Sequential Processing" = seq, "Parallel Processing" = par, times=100, unit="us")
mbm

#plot the microbenchmark results
library(ggplot2)

#box plot 
autoplot(mbm) + ggtitle("Autoplot of parallel and sequential processing time") +
  theme(plot.title = element_text(size=14, face="bold.italic"))

#bar chart
df_result <- data.frame(mbm)
p <- ggplot(data=df_result, aes(x=expr, y=time, fill=expr), legend.title="Dose (mg)") + geom_bar(stat="identity") +
  labs(title="Plot of Time by Processing Type", x ="Processing Type", 
       y = "Time (nanoseconds)", fill="Processing Type") +
  theme(plot.title = element_text(size=14, face="bold.italic"))
p


#extra comparison - method 2
#sequential processing
#record starting time
seq_start <- Sys.time()
#perform sequential processing
lapply(1:50, df)
#record ending time
seq_end <- Sys.time()

#calculate time duration
seq_duration <- seq_end - seq_start
seq_duration

#parallel processing
#create clusters for each cores
c2 <- makeCluster(numCores)
#add libraries to each clusters
clusterEvalQ(c2, {
  library(dplyr)
  library(tidyverse)
  library(data.table)
  library(lme4)
})

#record starting time
par_start <- Sys.time()
#run parallel processing
parLapply(c2, 1:50, df)
#record ending time
par_end <- Sys.time()

#calculate time duration
par_duration <- par_end - par_start
par_duration

#close cluster
stopCluster(c2)
