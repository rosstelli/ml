# https://en.wikibooks.org/wiki/Data_Mining_Algorithms_In_R/Classification/adaboost

install.packages("ada")
library("rpart")
library("ada")

setwd('../naive_bayes')
breast_cancer <- read.csv("dataset_13_breast-cancer.csv")
tmp <- rep(0, times=length(breast_cancer$Class))
tmp[as.numeric(breast_cancer$Class) == 1] <- -1
tmp[as.numeric(breast_cancer$Class) == 2] <- 1
breast_cancer$Class <- as.numeric(tmp)
setwd('../boost')

n <- nrow(breast_cancer)
ind <- sample(1:n)
trainval <- ceiling(n * .5)
testval <- ceiling(n * .3)
train <- breast_cancer[ind[1:trainval],]
test <- breast_cancer[ind[(trainval + 1):(trainval + testval)],]
valid <- breast_cancer[ind[(trainval + testval + 1):n],]

control <- rpart.control(cp = -1, maxdepth = 14,maxcompete = 1,xval = 0)
gen1 <- ada(Class~., data = train, test.x = test[,-10], test.y = test[,10], type = "gentle", control = control, iter = 700)
gen1 <- addtest(gen1, valid[,-10], valid[,10])
summary(gen1)
varplot(gen1)

#####################
# Old code
# data("soldat")
# n <- nrow(soldat)
# set.seed(100)
# ind <- sample(1:n)
# trainval <- ceiling(n * .5)
# testval <- ceiling(n * .3)
# train <- soldat[ind[1:trainval],]
# test <- soldat[ind[(trainval + 1):(trainval + testval)],]
# valid <- soldat[ind[(trainval + testval + 1):n],]
# 
# control <- rpart.control(cp = -1, maxdepth = 14,maxcompete = 1,xval = 0)
# gen1 <- ada(y~., data = train, test.x = test[,-73], test.y = test[,73], type = "gentle", control = control, iter = 70)
# gen1 <- addtest(gen1, valid[,-73], valid[,73])
# summary(gen1)
# varplot(gen1, max.var.show=40)
