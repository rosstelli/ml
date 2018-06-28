# Part of the code is here:
# https://www.r-bloggers.com/how-to-multinomial-regression-models-in-r/

setwd('../multinomial')
all_data <- read.csv("DATA NOT INCLUDED.csv")
# Data is not included
# Change the 'levels' to the levels in the dataset

error <- function (x, y) {
  return (sum(as.numeric(x) == as.numeric(y)))
}

####################################
# Multinomial logistic regression
####################################

library(nnet)
mod <- multinom(y ~ ., all_data)

predictions <- predict(mod)

predict(mod,all_data,"probs")

y_factors <- factor(all_data$y, levels=c(1,2,3,4), ordered=T)

num_right <- error(predictions, y_factors)

error_rate <- (length(predictions) - num_right) / length(predictions)

percent_right <- (num_right) / length(predictions)

####################################
# Ordinal logistic regression
####################################

require(foreign)
require(ggplot2)
require(MASS)
require(Hmisc)
require(reshape2)

all_data$y <- factor(all_data$y, levels=c(1,2,3,4), ordered=T)

olr <- polr(y ~ ., data=all_data, Hess=TRUE)

summary(olr)

ctable <- coef(summary(olr))

p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)

ci <- confint(olr)

exp(coef(olr))

exp(cbind(OR = coef(olr), ci))

summary(olr)
summary(update(olr, method = "probit", Hess = TRUE), digits = 3)
summary(update(olr, method = "logistic", Hess = TRUE), digits = 3)
summary(update(olr, method = "cloglog", Hess = TRUE), digits = 3)
addterm(olr, ~.^2, test = "Chisq")
m2 <- stepAIC(olr, ~.^2)
m2
summary(m2)
m2$anova
anova(olr, m2)


m3 <- update(olr, Hess=TRUE)
pr <- profile(m3)
confint(pr)

plot(pr)

pairs(pr)

Phat <- predict(olr, type="probs")

testing_results <- predict(olr, newdata=all_data[1:4, ], type="probs")

predictOLR <- function(model, newdata) {
  testing_results <- predict(model, newdata=newdata, type="probs")
  categories <- colnames(testing_results)
  indices <- apply(testing_results, 1, which.max)
  predicted_categories <- categories[indices]
  return (predicted_categories)
}

y2 <- predictOLR(olr,all_data)
all_data$y_predicted <- y2
print(all_data$y[1:12])
df2 <- cbind(all_data[2, ],y=y2)
y3 <- factor(as.numeric(y2), levels=c(1,2,3,4), ordered=T)
num_right_olr <- error(y3, all_data$y)
percent_right_olr <- (num_right_olr) / length(predictions)

print(paste("Percent right in Multinomial Logistic Regression:", percent_right*100))
print(paste("Percent right in Ordinal Logistic Regression:", percent_right_olr*100))

