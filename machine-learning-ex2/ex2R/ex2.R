## Machine Learning Online Class - Exercise 2: Logistic Regression
#
#  Instructions
#  ------------
# 
#  This file contains code that helps you get started on the logistic
#  regression exercise. You will need to complete the following functions 
#  in this exericse:
#
#     sigmoid.m
#     costFunction.m
#     predict.m
#     costFunctionReg.m
#
#  For this exercise, you will not need to change any code in this file,
#  or any other files other than those mentioned above.
#

## Initialization
#clear ; close all; clc
rm(list=ls())

## Load Data
#  The first two columns contains the exam scores and the third column
#  contains the label.
library(R.matlab)
library(matlab)


data <- read.table('../ex2/ex2data1.txt', sep=",")


X = data.matrix(data[, 1:2])
y = data.matrix(data[, 3])

## ==================== Part 1: Plotting ====================
#  We start the exercise by first plotting the data to understand the 
#  the problem we are working with.

source("plotData.R")

message(sprintf('Plotting data with + indicating (y = 1) examples and o '))
message(sprintf('indicating (y = 0) examples.\n'))

# Put some labels 
#hold on;
# Labels and Legend
# Specified in plot order

#hold off;
#xlabel('Exam 1 score')
#ylabel('Exam 2 score')
#legend('Admitted', 'Not admitted')

plotData(X, y)


readline(prompt='\nProgram paused. Press enter to continue.\n')


## ============ Part 2: Compute Cost and Gradient ============
#  In this part of the exercise, you will implement the cost and gradient
#  for logistic regression. You neeed to complete the code in 
#  costFunction.m

#  Setup the data matrix appropriately, and add ones for the intercept term

source("costFunction.R")

m = NROW(X) 
n = NCOL(X)

# Add intercept term to x and X_test
#X = [ones(m, 1) X];
X1 = cbind(rep(1, m), X)

# Initialize fitting parameters
initial_theta = matrix(0, n + 1, 1)

# Compute and display initial cost and gradient

JandG= costFunction(initial_theta, X1, y)
cost = JandG$J
grad = JandG$grad

message(sprintf('Cost at initial theta (zeros): %f\n', cost))
message(sprintf('Gradient at initial theta (zeros): \n'))
message(sprintf(' %f \n', grad))

readline(prompt='\nProgram paused. Press enter to continue.\n')


## ============= Part 3: Optimizing using fminunc  =============
#  In this exercise, you will use a built-in function (fminunc) to find the
#  optimal parameters theta.

source("fmincg.R")
source("plotDecisionBoundary.R")

#  Set options for fminunc
#options = optimset('GradObj', 'on', 'MaxIter', 400);

#  Run fminunc to obtain the optimal theta
#  This function will return theta and the cost 
#[theta, cost] = fminunc(@(t)(costFunction(t, X, y)), initial_theta, options);

TandC = fmincg(costFunction, initial_theta, Maxiter =400, X1, y)

theta <- TandC$par
cost <- TandC$cost

# Print theta to screen
message(sprintf('Cost at theta found by (fminunc) fmincg: %f\n', cost[length(cost)])) #last value
message(sprintf('theta: '))
message(sprintf(' %f \n', theta))

# Plot Boundary
plotDecisionBoundary(theta, X1, y)

# Put some labels 
#hold on;
# Labels and Legend
xlabel='Exam 1 score'
ylabel='Exam 2 score'

# Specified in plot order
legend("topright", c('Admitted', 'Not admitted'), pch=c(3,21), cex=1, col="black", pt.bg="yellow")
title(xlab = xlabel, ylab = ylabel)
#hold off;

readline(prompt='\nProgram paused. Press enter to continue.\n')

## ============== Part 4: Predict and Accuracies ==============
#  After learning the parameters, you'll like to use it to predict the outcomes
#  on unseen data. In this part, you will use the logistic regression model
#  to predict the probability that a student with score 45 on exam 1 and 
#  score 85 on exam 2 will be admitted.
#
#  Furthermore, you will compute the training and test set accuracies of 
#  our model.
#
#  Your task is to complete the code in predict.m

#  Predict probability for a student with score 45 on exam 1 
#  and score 85 on exam 2 
source("predict.R")

prob = sigmoid(c(1, 45, 85) %*% theta)
message(sprintf('For a student with scores 45 and 85, we predict an admission probability of %f\n\n', prob))

# Compute accuracy on our training set
p = predict(theta, X1)

message(sprintf('Train Accuracy: %f\n', mean(p == y) * 100))

readline(prompt='\nProgram paused. Press enter to continue.\n')

