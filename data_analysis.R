install.packages("GGally")
install.packages("pastecs")

library(ggplot2)
library(dplyr)
library(ggpubr)
library(GGally)
library(car)
library(pastecs)


#objective 1
#gender transaction data
gender_trans <-read.csv("C:/Sem6/5011CEM BigData/BigDataProject/gender_transactions.csv")
View(gender_trans)

#descriptive analysis
#display first 6 observations
head(gender_trans)

#display the structure of the dataset
str(gender_trans)

#compute the min, max, mean, median, 1st quartile and 3rd quartile for each attribute
summary(gender_trans)

#convert scientific notation numbers to non-scientific notation
options(scipen=100)
options(digits=3)

#additionally compute sum, range, standard deviation, variance and coefficients
#include skewness, kurtosis and normality test
stat.desc(gender_trans, norm = TRUE)

#histogram
#number of transactions
hist(gender_trans$num_transactions,
     main="Histogram of Number of Transactions",
     xlab="Number of Transactions",
     col="wheat")
abline(v = mean(gender_trans$num_transactions), col = 'red', lty = 2)

#male
hist(gender_trans$male,
     main="Histogram of Male Population",
     xlab="Male Population",
     col="wheat")
abline(v = mean(gender_trans$male), col = 'red', lty = 2)

#female
hist(gender_trans$female,
     main="Histogram of Female Population",
     xlab="Female Population",
     col="wheat")
abline(v = mean(gender_trans$female), col = 'red', lty = 2)

#population
hist(gender_trans$population,
     main="Histogram of Overall Population",
     xlab="Overall Population",
     col="wheat")
abline(v = mean(gender_trans$population), col = 'red', lty = 2)

#box plot
#boxplot(gender_trans$num_transactions)
#df <- data.frame()

#correlation test
mosthighlycorrelated <- function(mydataframe,numtoreport)
{
  # find the correlations
  cormatrix <- cor(mydataframe)
  # set the correlations on the diagonal or lower triangle to zero,
  # so they will not be reported as the highest ones:
  diag(cormatrix) <- 0
  cormatrix[lower.tri(cormatrix)] <- 0
  # flatten the matrix into a dataframe for easy sorting
  fm <- as.data.frame(as.table(cormatrix))
  # assign human-friendly names
  names(fm) <- c("First.Variable", "Second.Variable","Correlation")
  # sort and print the top n correlations
  head(fm[order(abs(fm$Correlation),decreasing=T),],n=numtoreport)
}

model <- select(gender_trans, 'num_transactions', 'male', 'female')
mosthighlycorrelated(model, 20)

#calculate correlation value
cor.test(formula = ~ num_transactions + male, data = gender_trans,
         method = "pearson", use = "complete.obs")
cor.test(formula = ~ num_transactions + female, data = gender_trans,
         method = "pearson", use = "complete.obs")

#create scatterplot matrix
ggpairs(gender_trans, columns = c( "male","female", "num_transactions"), 
        title = "Scatterplot of Transactions Number and Population Gender(Borough Area)", 
        upper = list(continuous = wrap("cor",size = 3)),
        lower = list(continuous = wrap("smooth",alpha = 1,size = 1,color = "darkcyan")))

#scatterplot for the correlation pairs
ggscatter(gender_trans, x = "male", y = "num_transactions", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Male", ylab = "Number of Transactions",
          add.params = list(color = "blue", fill = "lightgrey")) +
  ggtitle("Scatterplot of Male and Number of Transactions") +
  theme(plot.title = element_text(size=14, face="bold.italic"))

ggscatter(gender_trans, x = "female", y = "num_transactions", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Female", ylab = "Number of Transactions",
          add.params = list(color = "blue", fill = "lightgrey")) +
  ggtitle("Scatterplot of Female and Number of Transactions") +
  theme(plot.title = element_text(size=14, face="bold.italic"))

#hypothesis test
#calculate p-value
model_male <- lm(num_transactions ~ male, data = gender_trans)
summary(model_male)
model_female <- lm(num_transactions ~ female, data = gender_trans)
summary(model_female)

#regression 
#plot with fitted regression line
ggplot(gender_trans, aes(x=male, y=num_transactions)) +
  geom_point() +
  stat_smooth(method = "lm",  col = "red", fill = "yellowgreen") +
  stat_regline_equation() +
  theme_bw() + 
  labs(title = "Regression of Male over Number of Transactions",
       x = "Male", y = "Number of Transactions") +
  theme(plot.title = element_text(size=14, face="bold.italic"))

ggplot(gender_trans, aes(x=female, y=num_transactions)) +
  geom_point() +
  stat_smooth(method = "lm",  col = "red", fill = "yellowgreen") +
  stat_regline_equation() +
  theme_bw() + 
  labs(title = "Regression of Female over Number of Transactions",
       x = "Female", y = "Number of Transactions") +
  theme(plot.title = element_text(size=14, face="bold.italic"))



#objective 2
#age group transaction data
age_trans <-read.csv("C:/Sem6/5011CEM BigData/BigDataProject/age_group_transactions.csv")
View(age_trans)

#descriptive analysis
#display first 6 observations
head(age_trans)

#display the structure of the dataset
str(age_trans)

#compute the min, max, mean, median, 1st quartile and 3rd quartile for each attribute
summary(age_trans)

#additionally compute sum, range, standard deviation, variance and coefficients
#include skewness, kurtosis and normality test
stat.desc(age_trans, norm = TRUE)

#histogram
#number of transactions
hist(age_trans$num_transactions,
     main="Histogram of Number of Transactions",
     xlab="Number of Transactions",
     col="wheat")
abline(v = mean(age_trans$num_transactions), col = 'red', lty = 2)

#average age
hist(age_trans$avg_age,
     main="Histogram of Average Population Age",
     xlab="Average Population Age",
     col="wheat")
abline(v = mean(age_trans$avg_age), col = 'red', lty = 2)

#age_0_17
hist(age_trans$age_0_17,
     main="Histogram of Age Group 0 to 17",
     xlab="Age Group 0 to 17",
     col="wheat")
abline(v = mean(age_trans$age_0_17), col = 'red', lty = 2)

#age_18_64
hist(age_trans$age_18_64,
     main="Histogram of Age Group 18 to 64",
     xlab="Age Group 18 to 64",
     col="wheat")
abline(v = mean(age_trans$age_18_64), col = 'red', lty = 2)

#age_65+
hist(age_trans$age_65.,
     main="Histogram of Age Group 65 and Above",
     xlab="Age Group 65 and Above",
     col="wheat")
abline(v = mean(age_trans$age_65.), col = 'red', lty = 2)

#correlation test
model2 <- select(age_trans, 'num_transactions', 'age_0_17', 'age_18_64', 'age_65.', 'avg_age')
mosthighlycorrelated(model2, 20)

#create scatterplot matrix
ggpairs(age_trans, columns = c( "age_0_17","age_18_64", "age_65.", "avg_age","num_transactions"), 
        title = "Scatterplot of Transactions Number and Population Age Group(Borough Area)", 
        upper = list(continuous = wrap("cor",size = 3)),
        lower = list(continuous = wrap("smooth",alpha = 1,size = 1,color = "darkcyan")))

#calculate correlation value
cor.test(formula = ~ num_transactions + age_0_17, data = age_trans,
         method = "pearson", use = "complete.obs")
cor.test(formula = ~ num_transactions + age_18_64, data = age_trans,
         method = "pearson", use = "complete.obs")
cor.test(formula = ~ num_transactions + age_65., data = age_trans,
         method = "pearson", use = "complete.obs")
cor.test(formula = ~ num_transactions + avg_age, data = age_trans,
         method = "pearson", use = "complete.obs")

#scatterplot for the correlation pairs
ggscatter(age_trans, x = "avg_age", y = "num_transactions", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Average Age", ylab = "Number of Transactions",
          add.params = list(color = "blue", fill = "lightgrey")) +
  ggtitle("Scatterplot of Average Age and Number of Transactions") +
  theme(plot.title = element_text(size=14, face="bold.italic"))

ggscatter(age_trans, x = "age_0_17", y = "num_transactions", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Age Group 0 to 17", ylab = "Number of Transactions",
          add.params = list(color = "blue", fill = "lightgrey")) +
  ggtitle("Scatterplot of Age Group 0 to 17 and Number of Transactions") +
  theme(plot.title = element_text(size=14, face="bold.italic"))

ggscatter(age_trans, x = "age_18_64", y = "num_transactions", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Age Group 18 to 64", ylab = "Number of Transactions",
          add.params = list(color = "blue", fill = "lightgrey")) +
  ggtitle("Scatterplot of Age Group 18 to 64 and Number of Transactions") +
  theme(plot.title = element_text(size=14, face="bold.italic"))

ggscatter(age_trans, x = "age_65.", y = "num_transactions", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Age Group 65 and Above", ylab = "Number of Transactions",
          add.params = list(color = "blue", fill = "lightgrey")) +
  ggtitle("Scatterplot of Age Group 65 and Above and Number of Transactions") +
  theme(plot.title = element_text(size=14, face="bold.italic"))

#hypothesis test
#calculate p-value
model_age<- lm(num_transactions ~ avg_age, data = age_trans)
summary(model_age)
model_age2 <- lm(num_transactions ~ age_0_17, data = age_trans)
summary(model_age2)
model_age3 <- lm(num_transactions ~ age_18_64, data = age_trans)
summary(model_age3)
model_age4<- lm(num_transactions ~ age_65., data = age_trans)
summary(model_age4)

#regression 
#plot with fitted regression line
ggplot(age_trans, aes(x=avg_age, y=num_transactions)) +
  geom_point() +
  stat_smooth(method = "lm",  col = "red", fill = "yellowgreen") +
  stat_regline_equation() +
  theme_bw() + 
  labs(title = "Regression of Average Age over Number of Transactions",
       x = "Average Age", y = "Number of Transactions") +
  theme(plot.title = element_text(size=14, face="bold.italic"))

ggplot(age_trans, aes(x=age_0_17, y=num_transactions)) +
  geom_point() +
  stat_smooth(method = "lm",  col = "red", fill = "yellowgreen") +
  stat_regline_equation() +
  theme_bw() + 
  labs(title = "Regression of Age Group 0 to 17 over Number of Transactions",
       x = "Age Group 0 to 17", y = "Number of Transactions") +
  theme(plot.title = element_text(size=14, face="bold.italic"))

ggplot(age_trans, aes(x=age_18_64, y=num_transactions)) +
  geom_point() +
  stat_smooth(method = "lm",  col = "red", fill = "yellowgreen") +
  stat_regline_equation() +
  theme_bw() + 
  labs(title = "Regression of Age Group 18 to 64 over Number of Transactions",
       x = "Age Group 18 to 64", y = "Number of Transactions") +
  theme(plot.title = element_text(size=14, face="bold.italic"))

ggplot(age_trans, aes(x=age_65., y=num_transactions)) +
  geom_point() +
  stat_smooth(method = "lm",  col = "red", fill = "yellowgreen") +
  stat_regline_equation() +
  theme_bw() + 
  labs(title = "Regression of Age Group 65 and Above over Number of Transactions",
       x = "Age Group 65 and Above", y = "Number of Transactions") +
  theme(plot.title = element_text(size=14, face="bold.italic"))
