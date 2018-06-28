# From: http://blog.revolutionanalytics.com/2016/02/multivariate_data_with_r.html 
# SIMULATING MULTIVARIATE DATA
# https://stat.ethz.ch/pipermail/r-help/2003-September/038314.html
# lets first simulate a bivariate normal sample

distance <- function (x, y) {
  return (sqrt((x[1] - y[1]) * (x[1] - y[1]) + (x[2] - y[2]) * (x[2] - y[2])))
}

correct <- function (x, y) {
  return (sum(as.numeric(x) == as.numeric(y)))
}

setwd('../k_means')
library(MASS)
# Simulate bivariate normal data
mu <- c(0,0)                         # Mean
Sigma <- matrix(c(1, .1, .1, 1), 2)  # Covariance matrix
# > Sigma
# [,1] [,2]
# [1,]  1.0  0.1
# [2,]  0.1  1.0

# Generate sample from N(mu, Sigma)
bivn <- mvrnorm(100, mu = mu, Sigma = Sigma )  # from Mass package

# Second distribution
mu2 <- c(3,.4)                         # Mean
Sigma2 <- matrix(c(.5, -.4, -.4, 2), 2)  # Covariance matrix
bivn2 <- mvrnorm(100, mu = mu2, Sigma = Sigma2 )  # from Mass package

# Third distribution
mu3 <- c(1.5, 3)                         # Mean
Sigma3 <- matrix(c(1, 0, 0, 1), 2)  # Covariance matrix
bivn3 <- mvrnorm(100, mu = mu3, Sigma = Sigma3 )  # from Mass package

bivn_total <- rbind(bivn, bivn2)
bivn_total <- rbind(bivn_total, bivn3)
# Calculate kernel density estimate
bivn.kde <- kde2d(bivn_total[,1], bivn_total[,2], n = 100) #kde2d(bivn[,1], bivn[,2], n = 50)   # from MASS package

#Custom color palette 
heatmap_colors <- c("#FFFFFFFF", "#0040FFFF", "#0080FFFF", "#00BFFFFF", "#00FFBFFF", "#00FF80FF", "#00FF40FF", "#80FF00FF", "#B0FF00FF", "#FFB000FF", "#FFB00FFF", "#FF4000FF")
image(bivn.kde, col = heatmap_colors)       # from base graphics package
contour(bivn.kde, add = TRUE)     # from base graphics package


###############
# Sampling complete, k means
# data is stored in variable bivn_total
# https://www.statmethods.net/advstats/cluster.html

fit <- kmeans(bivn_total, 3)

library(cluster) 

# bivn_subset <- rbind(bivn_total[1:100, ], bivn_total[5001:5100, ], bivn_total[10001:10100, ])

clusplot(bivn_total, fit$cluster, color=TRUE, shade=TRUE, 
         labels=0, lines=0, main="Cluster of 3 Bivariate Distributions")


library(fpc)

plotcluster(bivn_total, fit$cluster)

# Estimate the accuracy

clusters <- c(0,0,0)

true_centers <- matrix(c(0, 0, 3, .4, 1.5, 3), nrow=3, ncol=2, byrow=TRUE)

# Simply grab the mean thats closest to a true mean
for (i in 1:length(true_centers[ ,1])) {
  cur_distance <- 1000000;
  cur_min <- 0;
  for (j in  1:length(fit$centers[,1])) {
    new_distance <- distance(fit$centers[j,], true_centers[i,])
    if (new_distance < cur_distance) {
      cur_min <- j
      cur_distance <- new_distance
    }
  }
  clusters[i] <- cur_min
}

true_values <- rep(0, times=300)
true_values[1:100] <- clusters[1]
true_values[101:200] <- clusters[2]
true_values[201:300] <- clusters[3]

number_correct <- correct(true_values, fit$cluster)

print(paste("Accuracy at identifying correct cluster =", (100 * number_correct / 300)))
