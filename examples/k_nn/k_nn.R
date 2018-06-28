# https://www.r-bloggers.com/k-nearest-neighbor-step-by-step-tutorial/
# https://rpubs.com/Nitika/kNN_Iris

setwd('../k_nn')
set.seed(1234)
correct <- function (x, y) {
  return (sum(as.numeric(x) == as.numeric(y)))
}


us_data <- read.csv("US Presidential Data.csv")#, stringsAsFactors = TRUE)
str(us_data)

library(class)

us_data_2 <- us_data[sample(nrow(us_data)),]
us_data_2.dup <- us_data_2
us_data_2$Win.Loss <- as.factor(us_data_2$Win.Loss)

num <- length(us_data_2$Win.Loss)
num_train <- (floor(0.7 * num))
num_test <- num - num_train
drops <- c("Win.Loss")
us_data_train <- us_data_2[1:num_train, ]
answers_train <- us_data_train$Win.Loss
us_data_test <- us_data_2[1:num_test + num_train, ]
answers_test <- us_data_test$Win.Loss
us_data_train <- us_data_train[ , !(names(us_data_train) %in% drops)]
us_data_test <- us_data_test[ , !(names(us_data_test) %in% drops)]

model <- knn(us_data_train, us_data_test, answers_train, k=11)

num_correct <- correct(model, answers_test)

print(paste("Accuracy =", (num_correct * 100 / num_test)))

###################################################

# install.packages('caret', dependencies = TRUE)
# install.packages('ddalpha') # SAY NO
# install.packages('kernlab') # SAY NO
library(caret)
library(e1071)

# set.seed(101)

# index = createDataPartition(data1$Win.Loss, p = 0.7, list = F )
train = us_data_2[1:num_train, ] #data1[index,]
validation = us_data_2[1:num_test + num_train, ] #data1[-index,]


levels(train$Win.Loss) <- make.names(levels(factor(train$Win.Loss)))
levels(validation$Win.Loss) <- make.names(levels(factor(validation$Win.Loss)))

repeats = 3
numbers = 10
tunel = 10

# set.seed(1234)
x = trainControl(method = "repeatedcv",
                 number = numbers,
                 repeats = repeats,
                 classProbs = TRUE,
                 summaryFunction = twoClassSummary)

model1 <- train(Win.Loss~. , data = train, method = "knn",
                preProcess = c("center","scale"),
                trControl = x,
                metric = "ROC",
                tuneLength = tunel)

# Summary of model
model1
plot(model1)

# Validation
valid_pred <- predict(model1,validation, type = "prob")

#Storing Model Performance Scores
library(ROCR)
pred_val <-prediction(valid_pred[,2],validation$Win.Loss)

# Calculating Area under Curve (AUC)
perf_val <- performance(pred_val,"auc")
print(perf_val)

# Plot AUC
perf_val <- performance(pred_val, "tpr", "fpr")
plot(perf_val, col = "green", lwd = 1.5)

#Calculating KS statistics
ks <- max(attr(perf_val, "y.values")[[1]] - (attr(perf_val, "x.values")[[1]]))
print(ks)

print(attr(pred_val,"predictions")[[1]])

predictions <- attr(pred_val,"predictions")[[1]]
values <- sort(unique(predictions))
for (i in 1:length(values)) {
  tmp <- (predictions >= values[i] ) + 1;
  num_correct <- correct(tmp, answers_test)
  print(paste("Accuracy at: ", i, " is ", (num_correct * 100 / length(tmp))));
}
