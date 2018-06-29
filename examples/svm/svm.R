# https://www.kaggle.com/ruslankl/mice-protein-expression/data
# https://www.r-bloggers.com/support-vector-machine-simplified-using-r/
# https://rstudio-pubs-static.s3.amazonaws.com/271792_96b51b7fa2af4b3f808d04f3f3051516.html 

setwd('../svm')
library(caret)
library(kernlab)
library(ROCR)

observations <- 15
samples <- 72

# data(segmentationData)

mice_data <- read.csv("Data_Cortex_Nuclear.csv") # Mice data
# remove_cols <- mice_data[sapply(mice_data, function(mice_data) !any(is.na(mice_data)))] # remove columns with NAs
select_first <- ((1:samples * observations) - (observations-1)) # 15 observations and 72 mice


# Randomly select the mice that are going to be the sample
indices <- createDataPartition(select_first, p=.7, list=FALSE)

# 
full_indices <- c();
for (i in (1:observations - 1)) {
  full_indices <- c(full_indices, (select_first[indices] + i))
}
full_indices <- sort(unique(full_indices));
zeros_nas <- mice_data # [select_first, ]
zeros_nas[is.na(zeros_nas)] <- 0 # Replace NAs with 0's
needed_data <- zeros_nas[ , 2:78]
num_iterations <- 1000

# for (i in 1:num_iterations) {
  
# Index <- createDataPartition(zeros_nas$Genotype,p=.7,list=FALSE)
svm.train <- zeros_nas[full_indices,]
svm.validate  <- zeros_nas[-full_indices,]
trainX <-svm.train[,2:78] 

# Setup for cross validation

ctrl <- trainControl(method="cv",
                     number = 2,
                     summaryFunction=twoClassSummary,
                     classProbs=TRUE)

# Grid search to fine tune SVM
grid <- expand.grid(sigma = c(.01, .015, 0.2),
                    C = c(0.75, 0.9, 1, 1.1, 1.25)
)

#Train SVM
svm.tune <- train(x=trainX,
                  y= svm.train$Genotype,
                  method = "svmRadial",
                  metric="ROC",
                  tuneGrid = grid,
                  trControl=ctrl)

print(svm.tune)

# Predict Target Label
valX <-svm.validate[,2:78]
pred <- predict(svm.tune, valX, type="prob")

# Model Performance Statistics
pred_val <-prediction(pred[,2], svm.validate$Genotype)

# Calculating Area under Curve
perf_val <- performance(pred_val,"auc")
perf_val

# Calculating True Positive and False Positive Rate
perf_val <- performance(pred_val, "tpr", "fpr")

# Plot the ROC curve
plot(perf_val, col = "green", lwd = 1.5)

#Calculating KS statistics
ks <- max(attr(perf_val, "y.values")[[1]] - (attr(perf_val, "x.values")[[1]]))
ks

# } # end of iterations
