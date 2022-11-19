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
cl <- makeCluster(numCores)
clusterEvalQ(cl, {
  library(dplyr)
  library(tidyverse)
  library(data.table)
  library(lme4)
})
par <- parLapply(cl, 1:50, df)
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
seq_start <- Sys.time()
lapply(1:50, df)
seq_end <- Sys.time()
seq_duration <- seq_end - seq_start
seq_duration

#parallel processing
c2 <- makeCluster(numCores)
clusterEvalQ(c2, {
  library(dplyr)
  library(tidyverse)
  library(data.table)
  library(lme4)
})

par_start <- Sys.time()
parLapply(c2, 1:50, df)
par_end <- Sys.time()
par_duration <- par_end - par_start
par_duration

stopCluster(c2)