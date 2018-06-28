setwd('../gmm/');

# Data and data normalization is omitted.

library(mixtools)

mixmdl = normalmixEM(data_full, k = K)

# Can read more at:
# https://www.r-bloggers.com/fitting-mixture-distributions-with-the-r-package-mixtools/
# https://www.r-bloggers.com/an-intro-to-gaussian-mixture-modeling/