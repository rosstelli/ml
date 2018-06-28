# Source: https://www.r-bloggers.com/naive-bayes-classification-in-r-part-2/
# Data: http://archive.ics.uci.edu/ml/datasets/Breast+Cancer?ref=datanews.io 

setwd('../naive_bayes')
breast_cancer <- read.csv("dataset_13_breast-cancer.csv")#, stringsAsFactors = TRUE)
#install.packages('e1071')
library(e1071)
model <- naiveBayes(Class ~ ., breast_cancer)
class(model)
summary(model)
print(model)

tbl_list <- sapply(breast_cancer[-10], table, breast_cancer[ , 10])
tbl_list <- lapply(tbl_list, t)

cond_probs <- sapply(tbl_list, function(x) { 
  apply(x, 1, function(x) { 
    x / sum(x) }) })

cond_probs <- lapply(cond_probs, t)

print(cond_probs)

preds <- predict(model, newdata = breast_cancer)

conf_matrix <- table(preds, breast_cancer$Class)

accuracy <- (conf_matrix[1,1] + conf_matrix[2,2]) / length(preds)

print(paste("Accuracy of Naive Bayes is:", accuracy * 100))
