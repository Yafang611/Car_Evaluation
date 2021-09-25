# Bayesian Methods - joint probabilities and conditional probabilities
library(naivebayes)
locmodel <- naive_bayes(decision ~ ., data = data1,laplace = 0.00001)
names(locmodel) # show the variables of the model
str(locmodel)

data3 <- data2 %>%
  mutate(predictions_Bayes = predict(locmodel, data1)) %>%
  mutate(accuracy_Bayes = (decision == mdl_selected))

View(data3)

# Build a multiclass confusion matrix with confusion_matrix()
library(cvms) # confusion_matrix(): Whereas evaluate() - another build a confusion matrix method - takes a data frame as input, confusion_matrix() takes a vector of targets and a vector of predictions.
confusion_mat_Bayes <- confusion_matrix(targets = data3$decision, predictions = data3$predictions_Bayes)
# The output includes the confusion matrix tibble and related metrics.
# plot the multiclass confusin matrix
str(confusion_mat_Bayes)
plot_confusion_matrix(confusion_mat_Bayes$`Confusion Matrix`[[1]])

#Adding sum titles: overall distribution of predictions and targets
plot_confusion_matrix(confusion_mat_Bayes$`Confusion Matrix`[[1]],
                      add_sums = TRUE,
                      palette = "Oranges")

conf_mat_Bayes <- tibble("Accuracy", "Sensitivity", "Specificity")
conf_mat_Bayes[1] <- confusion_mat_Bayes$`Overall Accuracy` # Balanced accuracy is a metric that one can use when evaluating how good a binary classifier is
conf_mat_Bayes[2] <- confusion_mat_Bayes$Sensitivity # TP/(TP+FN) 真正的结果是positive的，但是猜测有对有错
conf_mat_Bayes[3] <- confusion_mat_Bayes$Specificity # TN/(FP+TN) 真正的结果是negative的，但是猜测有对有错
conf_mat_Bayes

summary_evaluation <- rbind(conf_mat_KNN, conf_mat_Bayes)
