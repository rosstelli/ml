setwd('../logistic_regression')

# Function to make values between 0 and 1
normalize <- function (x) {
  minimum <- min(x)
  maximum <- max(x)
  tmp <- (x - minimum) / (maximum - minimum);
  return (tmp);
}

# For simplicity, make the cutoff point halfway
classify <- function (x) {
  tmp <- x;
  tmp[tmp>0.5] <- 1;
  tmp[tmp<=0.5] <- 0;
  return (tmp);
}

# Get any differences
calculate_error <- function (x, y) {
  tmp <- abs(x - y);
  return ((tmp));
}

# http://archive.ics.uci.edu/ml/machine-learning-databases/statlog/heart/ 
# https://www.r-bloggers.com/how-to-perform-a-logistic-regression-in-r/
# http://archive.ics.uci.edu/ml/datasets/heart+disease

# Load the data
training.data.raw <- read.csv('heart.dat',na.strings=c(""),sep=" ",stringsAsFactors = FALSE, header=FALSE)

sapply(training.data.raw,function(x) sum(is.na(x)))
sapply(training.data.raw, function(x) length(unique(x)))
colnames(training.data.raw) <- c("age","sex","chest.pain.type","resting.blood.pressure","serum.cholestoral",
                                 "fasting.blood.sugar","resting.electrocardiographic.results",
                                 "maximum.heart.rate","exercise.induced.angina",
                                 "oldpeak","slope.of.peak","number.of.vessels","thal","unknown.category")

#randomize
training.data.raw <- training.data.raw[sample(nrow(training.data.raw)), ]
training.data.raw$unknown.category <- as.factor(normalize(training.data.raw$unknown.category))
unknown.category <- training.data.raw$unknown.category

# want to predict unknown category
data <- subset(training.data.raw,select=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14))

print(dim(data)[1])

# Split the train and testing data
train <- data[1:200, ]
test <- data[201:270, ]

# Build the model
model <- glm(unknown.category ~.,family=binomial(link='logit'),data=train)
anova(model, test="Chisq")
fitted.results <- predict(model,newdata=test,type='response')
normalized.results <- classify(fitted.results);

# Calculate error
error <- sum(calculate_error(normalized.results, as.numeric(test[,14])-1))
print(paste("Accuracy = ", 1-(error / length(normalized.results))))
