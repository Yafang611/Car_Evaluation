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
  ggplot(aes(x = reorder(decision,n), y = n)) + geom_col() + #reorder(a,b): reorder a according to b
  labs(x = "decision",
       y = "count",
       title = "Count of Each Decision Group")

sum(is.na(data0))
summary(data0) # also can check the missing #

# scale is not possible because it is not numeric data. 
#data0_scaled <- scale(data0, center = TRUE, scale = TRUE)

vars0 <- colnames(data0)
vars1 <- vars0[!vars0 %in% "decision"]
# knn() can only do the numeric variables. If you want to do the categorical, you could (1) use caret instead of knn(), (2) convert categorical into dummy variables. Here we try (2)
library(fastDummies)
data1 <- data0 %>%
  dummy_cols(select_columns = vars1) %>%
  select(-vars1) # remove the original columns except for the decision columns.

data1_decision_dummy <- data1 %>%
  dummy_cols(select_columns = "decision")
View(data1_decision_dummy)

library(corrplot)
data1_decision_dummy %>%
  select(-decision) %>%
  cor()%>%
  corrplot()

View(data1)
str(data1)

set.seed(1988)
split <- sample(nrow(data1), nrow(data1)*0.8)
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
  predictions = knn(train = train[,-1], test = test[,-1], cl = train_decision, k = i)
  mean_values[i] <- mean(predictions == test_decision)
}

mean_values <- mean_values %>%
  rbind(k_value = c(1:10))
   
row.names(mean_values)[1] <- c("mean_values")

library(tidyverse) # convert rows to columns (or we can use t() function directly to do matrix transpose)
mean_values_convert <- as.data.frame(mean_values) %>%
  rownames_to_column() %>%
  gather(variable, value, -rowname) %>%
  spread(rowname, value)

str(mean_values_convert)
mean_values_convert <- mean_values_convert%>%
  arrange(k_value) %>%
  select(-variable)

library(ggplot2)
ggplot(mean_values_convert, aes(x = k_value, y = mean_values)) +
  geom_point() +
  geom_line() +
  scale_x_continuous(name = "K_value",breaks=seq(0,10,1)) +# to set the x-axis. seq() means from 0 to 10 and by = 2.
  labs(y = "Accuracy",
       title = "Accuracy vs. K values by KNN Model")

# Therefore, for this case, k = 8
mdl_selected <- knn(train = train[,-1], test = data1[,-1], cl = train_decision, k = 8)
data2 <- data1 %>%
  mutate(predictions_KNN = mdl_selected) %>%
  mutate(accuracy_KNN = (decision == mdl_selected))

View(data2)

data2 %>%
  select(accuracy_KNN) %>%
  summary() 


# Build a multiclass confusion matrix with confusion_matrix()
library(cvms) # confusion_matrix(): Whereas evaluate() - another build a confusion matrix method - takes a data frame as input, confusion_matrix() takes a vector of targets and a vector of predictions.
confusion_mat_KNN <- confusion_matrix(targets = data2$decision, predictions = data2$predictions_KNN)
# The output includes the confusion matrix tibble and related metrics.
# plot the multiclass confusin matrix
str(confusion_mat_KNN)
plot_confusion_matrix(confusion_mat_KNN$`Confusion Matrix`[[1]])

#Adding sum titles: overall distribution of predictions and targets
plot_confusion_matrix(confusion_mat_KNN$`Confusion Matrix`[[1]],
                      add_sums = TRUE,
                      palette = "Oranges")

conf_mat_KNN <- tibble("Accuracy", "Sensitivity", "Specificity")
conf_mat_KNN[1] <- confusion_mat_KNN$`Overall Accuracy` # Balanced accuracy is a metric that one can use when evaluating how good a binary classifier is
conf_mat_KNN[2] <- confusion_mat_KNN$Sensitivity # TP/(TP+FN) 真正的结果是positive的，但是猜测有对有错
conf_mat_KNN[3] <- confusion_mat_KNN$Specificity # TN/(FP+TN) 真正的结果是negative的，但是猜测有对有错
conf_mat_KNN

#rename the column names (rename(new_name = old_name))
conf_mat_KNN <- conf_mat_KNN %>%
  rename("Accuracy" = "\"Accuracy\"",
         "Sensitivity" = "\"Sensitivity\"",
         "Specificity" = "\"Specificity\"")
conf_mat_KNN
  