#
# 
#
# Machine Learning Online Class
#  Exercise 8 | Anomaly Detection and Collaborative Filtering
#
#  Instructions
#  ------------
#
#  This file contains code that helps you get started on the
#  exercise. You will need to complete the following functions:
#
#     estimateGaussian.m
#     selectThreshold.m
#     cofiCostFunc.m
#
#  For this exercise, you will not need to change any code in this file,
#  or any other files other than those mentioned above.
#

rm(list=ls())

## ================== Part 1: Load Example Dataset  ===================
#  We start this exercise by using a small dataset that is easy to
#  visualize.
#
#  Our example case consists of 2 network server statistics across
#  several machines: the latency and throughput of each machine.
#  This exercise will help us find possibly faulty (or very fast) machines.
#

#fprintf('Visualizing example dataset for outlier detection.\n\n')

message(sprintf("Visualizing example dataset for outlier detection.\n\n"))
library(R.matlab)


#  The following command loads the dataset. You should now have the
#  variables X, Xval, yval in your environment
data <- readMat("../ex8/ex8data1.mat")

X <- data$X
Xval <- data$Xval
yval <- data$yval

#  Visualize the example dataset

plot(X[,1], X[,2], pch=4, col="blue", xlim=c(0,35), ylim=c(0,35))

message(sprintf("program pause\n"))

readline(prompt="press enter to continue \n")


# ================== Part 2: Estimate the dataset statistics ===================
#  For this exercise, we assume a Gaussian distribution for the dataset.
#
#  We first estimate the parameters of our assumed Gaussian distribution, 
#  then compute the probabilities for each of the points and then visualize 
#  both the overall distribution and where each of the points falls in 
#  terms of that distribution.
#
message(sprintf('Visualizing Gaussian fit.\n\n'))

#  Estimate my and sigma2
#
# estimate Gaussian
#
mu <- matrix(colMeans(X), 1)
sig <- matrix(apply(X, 2, var), 1)

#  Returns the density of the multivariate normal at each data point (row) 
#  of X

source("multivariateGaussian.R")

p <- multivariateGaussian(X, mu, sig)

#  Visualize the fit
source("visualizeFit.R")
visualizeFit(X, mu, sig)

message(sprintf('Program paused. Press enter to continue.\n'))


## ================== Part 3: Find Outliers ===================
#  Now you will find a good epsilon threshold using a cross-validation set
#  probabilities given the estimated Gaussian distribution
# 


#
# find outliner
#
pval <- multivariateGaussian(Xval, mu, sig)

source("selectThreshhold.R")
best <- selectThreshhold(yval, pval)

F1 <- best$bestF1
epsilon <- best$bestEpsilon

message(sprintf('Best epsilon found using cross-validation: %e\n', epsilon))
message(sprintf('Best F1 on Cross Validation Set:  %f\n', F1))
message(sprintf('   (you should see a value epsilon of about 8.99e-05)\n\n'))

#  Find the outliers in the training set and plot the
outliner <- which(p < epsilon)

#  Draw a red circle around those outliers
#plot(X[,1], X[,2], pch=4, col="blue")

points(X[outliner, ], pch=1, col = "red", cex=2.5, xlim=c(0,35), ylim=c(0,35))

readline(prompt="Program pause, press enter to continue")


## ================== Part 4: Multidimensional Outliers ===================
#  We will now use the code from the previous part and apply it to a 
#  harder problem in which more features describe each datapoint and only 
#  some features indicate whether a point is an outlier.
#

#  Loads the second dataset. You should now have the
#  variables X, Xval, yval in your environment

data <- readMat("../ex8/ex8data2.mat")
X <- data$X
Xval <- data$Xval
yval <- data$yval


#  Apply the same steps to the larger dataset
#[mu sigma2] = estimateGaussian(X);
mu <- matrix(colMeans(X), 1)
sigma2 <- matrix(apply(X, 2, var), 1)


#  Training set 
p = multivariateGaussian(X, mu, sigma2)

#  Cross-validation set
pval = multivariateGaussian(Xval, mu, sigma2)

#  Find the best threshold
best = selectThreshhold(yval, pval)

F1 <- best$bestF1
epsilon <- best$bestEpsilon

message(sprintf('Best epsilon found using cross-validation: %e\n', epsilon))
message(sprintf('Best F1 on Cross Validation Set:  %f\n', F1))
message(sprintf('# Outliers found: %d\n', sum(p < epsilon)))
message(sprintf('   (you should see a value epsilon of about 1.38e-18)\n\n'))



