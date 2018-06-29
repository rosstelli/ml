##############################################################################
# Original Code and data:
# https://www.r-bloggers.com/fitting-a-neural-network-in-r-neuralnet-package/
# https://www.kaggle.com/uciml/breast-cancer-wisconsin-data/data 
##############################################################################

setwd('../nn')
# Load in data from kaggle
breast_cancer <- read.csv("breast_cancer_2.csv")
breast_cancer[, 1] <- NULL; # Remove first column
breast_cancer$X <- NULL;    # Remove unnecessary column
tmp <- rep(0, times=length(breast_cancer$diagnosis))
tmp[as.numeric(breast_cancer$diagnosis) == 1] <- -1
tmp[as.numeric(breast_cancer$diagnosis) == 2] <- 1
breast_cancer$diagnosis <- as.numeric(tmp)

# Sample data
index <- sample(1:nrow(breast_cancer),round(0.75*nrow(breast_cancer)))
train <- breast_cancer[index,]
test <- breast_cancer[-index,]

maxs <- apply(breast_cancer, 2, max) 
mins <- apply(breast_cancer, 2, min)
library(MASS)

# Normalize Data
scaled <- as.data.frame(scale(breast_cancer, center = mins, scale = maxs - mins))
train_ <- scaled[index,]
test_ <- scaled[-index,]

################################
# Comparison
lm.fit <- glm(diagnosis~., data=train_)
summary(lm.fit)
pr.lm <- predict(lm.fit,test_)
MSE.lm <- sum((pr.lm - test_$diagnosis)^2)/nrow(test_)

################################
# Small neural network

library(neuralnet)
n <- names(train_)
f <- as.formula(paste("diagnosis ~", paste(n[!n %in% "diagnosis"], collapse = " + ")))
nn <- neuralnet(f,data=train_,hidden=c(5,3),linear.output=T)
plot(nn)
pr.nn <- compute(nn, test_[,(2:ncol(test_))])
pr.nn_ <- pr.nn$net.result*(max(breast_cancer$diagnosis)-min(breast_cancer$diagnosis))+min(breast_cancer$diagnosis)
test.r <- (test_$diagnosis)*(max(breast_cancer$diagnosis)-min(breast_cancer$diagnosis))+min(breast_cancer$diagnosis)
MSE.nn <- sum((test.r - pr.nn_)^2)/nrow(test_)
print(MSE.nn)

##################################################
# Larger Neural Network

nn <- neuralnet(f,data=train_,hidden=c(29,25,21,17,13,9,7,5,3),linear.output=T)
# plot(nn)
pr.nn <- compute(nn, test_[,(2:ncol(test_))])
pr.nn_ <- pr.nn$net.result*(max(breast_cancer$diagnosis)-min(breast_cancer$diagnosis))+min(breast_cancer$diagnosis)
test.r <- (test_$diagnosis)*(max(breast_cancer$diagnosis)-min(breast_cancer$diagnosis))+min(breast_cancer$diagnosis)
MSE.nn <- sum((test.r - pr.nn_)^2)/nrow(test_)
print(MSE.nn)
