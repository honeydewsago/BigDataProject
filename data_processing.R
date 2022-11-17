install.packages("tidyverse")
install.packages("microbenchmark")


library(parallel)
library(microbenchmark)
library(dplyr)

numCores <- detectCores() 
detectCores(logical = FALSE)

setwd("C:/Sem6/5011CEM BigData/BigDataProject/datasets/")

#read files using data.table package
library(tidyverse)
library(data.table)

df <- function(i){
  list.files(path = "C:/Sem6/5011CEM BigData/BigDataProject/datasets/", pattern = "*.csv") %>%
  map_df(~fread(.))
}
df

#sequential processing
#method 1
system.time(lapply(1:5, df))

#method 2
starttime <- Sys.time()
lapply(1:2, df)
endtime <- Sys.time()
duration <- endtime - starttime
duration


#parallel processing
#using sockets
cl <- makeCluster(numCores)
clusterEvalQ(cl, {
  library(dplyr)
  library(tidyverse)
  library(data.table)
  library(lme4)
})
system.time(parLapply(cl, 1:5, df))
stopCluster(cl)

#microbenchmark
cl <- makeCluster(numCores)
clusterEvalQ(cl, {
  library(dplyr)
  library(tidyverse)
  library(data.table)
  library(lme4)
})

seq <- lapply(1:10, df)
par <- parLapply(cl, 1:10, df)

mbm <- microbenchmark(seq, par, times=1)
mbm
stopCluster(cl)

#autoplot(mbm)

df <- data.frame(mbm)
p <- ggplot(data=df, aes(x=expr, y=time)) + geom_bar(stat="identity")
p
