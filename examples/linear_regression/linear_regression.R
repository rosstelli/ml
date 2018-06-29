setwd('../linear_regression')

# http://lawcenter.giffords.org/scorecard/

all_data <- read.csv("gun_data.csv")

linear_regression_2 <- lm(GUN.DEATH.RATE ~ GUN.LAW.STRENGTH..RANKED. , data=all_data)

plot(all_data$GUN.LAW.STRENGTH..RANKED., all_data$GUN.DEATH.RATE, col='blue', pch=20, cex=2, main="Relationship between Gun Laws and Gun Death rate per capita", xlab="Rank", ylab="Death Rate per Capita")
abline(linear_regression_2)
