## Machine Learning Online Class
#  Exercise 1: Linear regression with multiple variables
#
#  Instructions
#  ------------
# 
#  This file contains code that helps you get started on the
#  linear regression exercise. 
#
#  You will need to complete the following functions in this 
#  exericse:
#
#     warmUpExercise.m
#     plotData.m
#     gradientDescent.m
#     computeCost.m
#     gradientDescentMulti.m
#     computeCostMulti.m
#     featureNormalize.m
#     normalEqn.m
#
#  For this part of the exercise, you will need to change some
#  parts of the code below for various experiments (e.g., changing
#  learning rates).
#

## Initialization

## ================ Part 1: Feature Normalization ================
    
## Clear and Close Figures
##clear ; close all; clc

rm(list=ls())

message(sprintf('Loading data ...\n'))

## Load Data


#data = load('ex1data2.txt');
#X = data(:, 1:2);
#y = data(:, 3);

data = read.table('../ex1/ex1data2.txt', sep=",")

#X = data(:, 1); y = data(:, 2);
X = data.matrix(data[, 1:2])
y = data.matrix(data[, 3])
m = length(y)

#x_mean = zeros(1, size(X, 2));
#x_std = zeros(1, size(X, 2));


# Print out some data points
message(sprintf('First 10 examples from the dataset: \n'))

cat(sprintf(' x = [%.0f %.0f], y = %.0f ', X[1:10,1], X[1:10, 2], y[1:10,]), sep="\n")
        
readline(prompt='Program paused. Press enter to continue.\n')
        
# Scale features and set them to zero mean
source("featureNormalize.R")
message(sprintf('Normalizing Features ...\n'))

x_mean = colMeans(X)
x_std = apply(X, 2, std)
        
cat(sprintf("%0.f\n", x_mean))

normX = featureNormalize(X)

X = normX$X_norm
mu = normX$mu
sigma = normX$sigma
        
# Add intercept term to X
#X = [ones(m, 1) X];
 
X1 = cbind(rep(1, m), X)
        
## ================ Part 2: Gradient Descent ================
        
# ====================== YOUR CODE HERE ======================
# Instructions: We have provided you with the following starter
#               code that runs gradient descent with a particular
#               learning rate (alpha). 
#
#               Your task is to first make sure that your functions - 
#               computeCost and gradientDescent already work with 
#               this starter code and support multiple variables.
#
#               After that, try running gradient descent with 
#               different values of alpha and see which one gives
#               you the best result.
#
#               Finally, you should complete the code at the end
#               to predict the price of a 1650 sq-ft, 3 br house.
#
# Hint: By using the 'hold on' command, you can plot multiple
#       graphs on the same figure.
#
# Hint: At prediction, make sure you do the same feature normalization.
#

source("gradientDescentMulti.R")
message(sprintf('Running gradient descent ...\n'))
        
# Choose some alpha value
alpha = 0.3
num_iters = 50
        
# Init Theta and Run Gradient Descent 
theta = rep(0, 3)    #zeros(3, 1);
    
TandJ = gradientDescentMulti(X1, y, theta, alpha, num_iters)
theta = TandJ$theta
J1 = TandJ$J_history
        
# Choose another alpha value
alpha = 0.1
num_iters = 50
        
# Init Theta and Run Gradient Descent 
theta = rep(0, 3)    #zeros(3, 1);

TandJ = gradientDescentMulti(X1, y, theta, alpha, num_iters)
theta = TandJ$theta
J2 = TandJ$J_history

# Choose another alpha value
alpha = 0.03
num_iters = 100
        
# Init Theta and Run Gradient Descent 
theta = rep(0,3)   #zeros(3, 1);

TandJ= gradientDescentMulti(X1, y, theta, alpha, num_iters)
theta = TandJ$theta
J3 = TandJ$J_history
        
# Plot the convergence graph
#        figure;

#plot(1:numel(J1), J1, '-b', 'LineWidth', 2)
xlim = c(0, num_iters)
ylim = c(min(J1), max(J1))
plot(1:numel(J1), J1, type="l", col="blue", lwd=2, xlab="", ylab="", xlim=xlim, ylim=ylim)
xlabel='Number of iterations'
ylabel='Cost J'
title(xlab=xlabel, ylab = ylabel)
#hold on;

par(new=TRUE)
#plot(1:numel(J2), J2, 'r', 'LineWidth', 2);
plot(1:numel(J2), J2, type="l", col="red", lwd=2, xlab="", ylab="", xlim=xlim, ylim=ylim)

par(new=TRUE)
#plot(1:numel(J3), J3, 'k', 'LineWidth', 2);
plot(1:numel(J3), J3, type="l", col="black", lwd=2, xlab="", ylab="", xlim = xlim, ylim=ylim)        
        
# Display gradient descent's result
message(sprintf('Theta computed from gradient descent: \n'))
message(sprintf(' %f \n', theta))
message(sprintf('\n'))
        
# Estimate the price of a 1650 sq-ft, 3 br house
# ====================== YOUR CODE HERE ======================
# Recall that the first column of X is all-ones. Thus, it does
# not need to be normalized.
        
P = c(((1650 - x_mean[1])/ x_std[1]) , ((3 - x_mean[2])/x_std[2]))
        
P = c(1, P)
        
price = t(theta) %*% P   # You should change this
        
        
# ============================================================
        
message(sprintf('Predicted price of a 1650 sq-ft, 3 br house '))
message(sprintf('(using gradient descent):\n $%f\n', price))
        
readline(prompt='Program paused. Press enter to continue.\n')
        
## ================ Part 3: Normal Equations ================
        
message(sprintf('Solving with normal equations...\n'))
        
# ====================== YOUR CODE HERE ======================
# Instructions: The following code computes the closed form 
#               solution for linear regression using the normal
#               equations. You should complete the code in 
#               normalEqn.m
#
#               After doing so, you should complete this code 
#               to predict the price of a 1650 sq-ft, 3 br house.
#

source("normalEqn.R")
# Load Data
#data = csvread('ex1data2.txt');
#X = data(:, 1:2);
#y = data(:, 3);


data = read.table('../ex1/ex1data2.txt', sep=",")

#X = data(:, 1); y = data(:, 2);
X = data.matrix(data[, 1:2])
y = data.matrix(data[, 3])

m = length(y)

       
# Add intercept term to X
#X = [ones(m, 1) X];
 
X1 = cbind(rep(1,m), X)

# Calculate the parameters from the normal equation
theta = normalEqn(X1, y)
        
# Display normal equation's result
message(sprintf('Theta computed from the normal equations: \n'))
message(sprintf(' %f \n', theta))
message(sprintf('\n'))
        
        
# Estimate the price of a 1650 sq-ft, 3 br house
# ====================== YOUR CODE HERE ======================
P = c(1, 1650, 3)
        
price = t(theta) %*% P   # You should change this
        
        
# ============================================================
        
message(sprintf('Predicted price of a 1650 sq-ft, 3 br house '))
message(sprintf('(using normal equations):\n $%f\n', price))
        
        