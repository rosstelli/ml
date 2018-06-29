# https://www.statmethods.net/advstats/cart.html 

# Classification Tree with rpart
library(rpart)
setwd('../naive_bayes')
breast_cancer <- read.csv("dataset_13_breast-cancer.csv")
tmp <- rep("", times=length(breast_cancer$Class))
tmp[as.numeric(breast_cancer$Class) == 1] <- 'none'
tmp[as.numeric(breast_cancer$Class) == 2] <- 'recurrence'
breast_cancer$Class <- as.factor(tmp)
setwd('../trees')

# grow tree 
fit <- rpart(Class ~ .,
             method="class", data=breast_cancer)

printcp(fit) # display the results 
plotcp(fit) # visualize cross-validation results 
summary(fit) # detailed summary of splits

# plot tree 
plot(fit, uniform=TRUE, 
     main="Classification Tree for Cancer Relapse")
text(fit, use.n=TRUE, all=TRUE, cex=0.7)

# create attractive postscript plot of tree 
post(fit, file = "tree_cancer.ps", 
     title = "Classification Tree for Cancer Relapse")

# prune the tree 
pfit<- prune(fit, cp=   fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"])

# plot the pruned tree 
plot(pfit, uniform=TRUE, 
     main="Pruned Classification Tree for Cancer Relapse")
text(pfit, use.n=TRUE, all=TRUE, cex=.8)
post(pfit, file = "ptree.ps", 
     title = "Pruned Classification Tree for Cancer Relapse")

