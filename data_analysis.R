install.packages("GGally")

library(ggplot2)
library(dplyr)
library(GGally)
library(car)

#objective 1
#gender transaction data
gender_transactions <-read.csv("C:/Sem6/5011CEM BigData/BigDataProject/gender_transactions.csv")
View(gender_transactions)
summary(gender_transactions)

#correlation test
mosthighlycorrelated <- function(mydataframe,numtoreport)
{
  cormatrix <- cor(mydataframe)
  diag(cormatrix) <- 0
  cormatrix[lower.tri(cormatrix)] <- 0
  fm <- as.data.frame(as.table(cormatrix))
  names(fm) <- c("First.Variable", "Second.Variable","Correlation")
  head(fm[order(abs(fm$Correlation),decreasing=T),],n=numtoreport)
}

model <- select(gender_transactions, 'num_transactions', 'male', 'female')
mosthighlycorrelated(model, 20)

#calculate correlation value
cor.test(formula = ~ num_transactions + male, data = gender_transactions,
         method = "pearson", use = "complete.obs")
cor.test(formula = ~ num_transactions + female, data = gender_transactions,
         method = "pearson", use = "complete.obs")

#create scatterplot matrix
ggpairs(gender_transactions, columns = c( "male","female", "num_transactions"), 
        title = "Scatterplot of transactions number and populataion gender(Borough Area)", 
        upper = list(continuous = wrap("cor",size = 3)),
        lower = list(continuous = wrap("smooth",alpha = 1,size = 1,color = "blue")))

#create scatterplot with fitted regression line
ggplot(gender_transactions, aes(x = male, y = num_transactions)) +
  geom_point() +
  stat_smooth(method = "lm")
ggplot(gender_transactions, aes(x = female, y = num_transactions)) +
  geom_point() +
  stat_smooth(method = "lm")

#hypothesis test
#calculate p-value
model<- lm(num_transactions ~ male, data = gender_transactions)
summary(model)
model<- lm(num_transactions ~ female, data = gender_transactions)
summary(model)

#plot ?
plot(model)



#objective 2
#age group transaction data
age_transactions <-read.csv("C:/Sem6/5011CEM BigData/BigDataProject/age_group_transactions.csv")
View(age_transactions)
summary(age_transactions)

#correlation test
model <- select(age_transactions, 'num_transactions', 'age_0_17', 'age_18_64', 'age_65.', 'avg_age')
mosthighlycorrelated(model, 20)

#calculate correlation value
cor.test(formula = ~ num_transactions + age_0_17, data = age_transactions,
         method = "pearson", use = "complete.obs")
cor.test(formula = ~ num_transactions + age_18_64, data = age_transactions,
         method = "pearson", use = "complete.obs")
cor.test(formula = ~ num_transactions + age_65., data = age_transactions,
         method = "pearson", use = "complete.obs")
cor.test(formula = ~ num_transactions + avg_age, data = age_transactions,
         method = "pearson", use = "complete.obs")

#create scatterplot matrix
ggpairs(age_transactions, columns = c( "age_0_17","age_18_64", "age_65.", "avg_age","num_transactions"), 
        title = "Scatterplot of transactions number and population age(Borough Area)", 
        upper = list(continuous = wrap("cor",size = 3)),
        lower = list(continuous = wrap("smooth",alpha = 1,size = 1,color = "blue")))

#create scatterplot with fitted regression line
ggplot(age_transactions, aes(x = age_0_17, y = num_transactions)) +
  geom_point() +
  stat_smooth(method = "lm")
ggplot(age_transactions, aes(x = age_18_64, y = num_transactions)) +
  geom_point() +
  stat_smooth(method = "lm")
ggplot(age_transactions, aes(x = age_65., y = num_transactions)) +
  geom_point() +
  stat_smooth(method = "lm")
ggplot(age_transactions, aes(x = avg_age, y = num_transactions)) +
  geom_point() +
  stat_smooth(method = "lm")


#hypothesis test
#calculate p-value
model<- lm(num_transactions ~ age_0_17, data = age_transactions)
summary(model)
model<- lm(num_transactions ~ age_18_64, data = age_transactions)
summary(model)
model<- lm(num_transactions ~ age_65., data = age_transactions)
summary(model)
model<- lm(num_transactions ~ avg_age, data = age_transactions)
summary(model)

#plot ?
plot(model)

