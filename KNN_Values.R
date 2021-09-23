rm(list = is())
setwd("C:/Users/vicky/OneDrive/桌面/Coding and Data Literacy/Kaggle/CarEvaluationData")
data0 <- read.csv("car_evaluation.csv")

library(dplyr)
colnames(data0) <- c("buyingCost", "maintenanceCost", "#Doors", "#Persons", "lugBoot", "Safety", "decision")
View(data0)

library(ggplot2)
library(forcats) #fct_reorder()
data0 %>%
  count(decision) %>%
  ggplot(aes(x = reorder(decision,n), y = n)) + geom_col() #reorder(a,b): reorder a according to b

sum(is.na(data0))
summary(data0)

# scale is not possible because it is not numeric data. 
#data0_scaled <- scale(data0, center = TRUE, scale = TRUE)

vars0 <- colnames(data0)
vars1 <- vars0[!vars0 %in% "decision"]
# knn() can only do the numeric variables. If you want to do the categorical, you could (1) use caret instead of knn(), (2) convert categorical into dummy variables. Here we try (2)
library(fastDummies)
data1 <- data0 %>%
  dummy_cols(select_columns = vars1) %>%
  select(-vars1) # remove the original columns except for the decision columns.
View(data1)


split <- sample(nrow(data1)*0.8)
train <- data1[split,]
test <- data1[-split,]
train_decision <- train[,1]
test_decision <- test[,1]

#calculate the baseline model(default k=1)
library(class) #knn()
# knn() can only do the numeric variables. If you want to do the categorical, you could (1) use caret instead of knn(), (2) convert categorical into dummy variables
mean_values <- 0
explanatory_data <- tibble(k_value = 1:10)
for(i in 1:10){
  k_value = knn(train = train[,-1], test = test[,-1], cl = train_decision, k = i)
  mean_values[i] <- mean(k_value == test_decision)
}

mean_values <- mean_values %>%
  rbind(k_value = c(1:10))
   
row.names(mean_values)[1] <- c("mean_values")

library(tidyverse) # convert rows to columns
mean_values_convert <- as.data.frame(mean_values) %>%
  rownames_to_column() %>%
  gather(variable, value, -rowname) %>%
  spread(rowname, value)

library(ggplot2)
ggplot(mean_values_convert, aes(x = k_value, y = mean_values)) +
  geom_point() +
  geom_line() 
