## Machine Learning Online Class - Exercise 2: Logistic Regression
#
#  Instructions
#  ------------
# 
#  This file contains code that helps you get started on the second part
#  of the exercise which covers regularization with logistic regression.
#
#  You will need to complete the following functions in this exericse:
#
#     sigmoid.m
#     costFunction.m
#     predict.m
#     costFunctionReg.m
#
#  For this exercise, you will not need to change any code in this file,
#  or any other files other than those mentioned above.
#
library(R.matlab)
library(matlab)

## Initialization
#clear ; close all; clc
rm(list=ls())


# Load Data
#  The first two columns contains the X values and the third column
#  contains the label (y).
source("plotData.R")
data <- read.table('../ex2/ex2data2.txt', sep=",")

#X = data(:, [1, 2]); y = data(:, 3);

X = data.matrix(data[, 1:2])
y = data.matrix(data[, 3])

plotData(X, y)

# Put some labels 
#hold on;

# Labels and Legend
xlabel = "Microchip Test 1"
ylabel = "Microchip Test 2"
title(xlab=xlabel, ylab= ylabel)

# Specified in plot order
legend("topright", c('y = 1', 'y = 0'),  pch=c(3,21), cex=1, col="black", pt.bg="yellow")

#hold off;


## =========== Part 1: Regularized Logistic Regression ============
#  In this part, you are given a dataset with data points that are not
#  linearly separable. However, you would still like to use logistic 
#  regression to classify the data points. 
#
#  To do so, you introduce more features to use -- in particular, you add
#  polynomial features to our data matrix (similar to polynomial
#  regression).
#

# Add Polynomial Features

# Note that mapFeature also adds a column of ones for us, so the intercept
# term is handled
source("costFunctionReg.R")
source("mapFeature.R")

X = mapFeature(X[,1], X[,2])

# Initialize fitting parameters
initial_theta = rep(0, NCOL(X))               #zeros(size(X, 2), 1)

# Set regularization parameter lambda to 1
lambda = 1

# Compute and display initial cost and gradient for regularized logistic
# regression

JandG = costFunctionReg(initial_theta, X, y, lambda)

cost <- JandG$J
grad <- JandG$grad

message(sprintf('Cost at initial theta (zeros): %f\n', cost))

readline(prompt='\nProgram paused. Press enter to continue.\n')


## ============= Part 2: Regularization and Accuracies =============
#  Optional Exercise:
#  In this part, you will get to try different values of lambda and 
#  see how regularization affects the decision coundart
#
#  Try the following values of lambda (0, 1, 10, 100).
#
#  How does the decision boundary change when you vary lambda? How does
#  the training set accuracy vary?
#
source("fmincg.R")
source("plotDecisionBoundary.R")
source("predict.R")


# Initialize fitting parameters
initial_theta = rep(0, NCOL(X))  #zeros(size(X, 2), 1)

# Set regularization parameter lambda to 1 (you should vary this)
lambda = 1

# Set Options
#options = optimset('GradObj', 'on', 'MaxIter', 400);

# Optimize
#[theta, J, exit_flag] = fminunc(@(t)(costFunctionReg(t, X, y, lambda)), initial_theta, options)

TandJ = fmincg(costFunctionReg, initial_theta, Maxiter=400, X, y, lambda)

theta <- TandJ$par

# Plot Boundary
plotDecisionBoundary(theta, X, y)
#hold on;
title = sprintf('lambda = %g', lambda)

# Labels and Legend
xlabel = 'Microchip Test 1'
ylabel = 'Microchip Test 2'

title(xlab=xlabel, ylab=ylabel, main=title)

legend("topright", c('y = 1', 'y = 0'), title='Decision boundary', cex=0.5, pch=c(3,21),  col="black", pt.bg="yellow")
#hold off;

# Compute accuracy on our training set
p = predict(theta, X)

message(sprintf('Train Accuracy: %f\n', mean(p == y) * 100))


