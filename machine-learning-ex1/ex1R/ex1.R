## Machine Learning Online Class - Exercise 1: Linear Regression

#  Instructions
#  ------------
# 
#  This file contains code that helps you get started on the
#  linear exercise. You will need to complete the following functions 
#  in this exericse:
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
#  For this exercise, you will not need to change any code in this file,
#  or any other files other than those mentioned above.
#
# x refers to the population size in 10,000s
# y refers to the profit in $10,000s
#

## Initialization
#clear ; close all; clc

rm(list=ls())


## ==================== Part 1: Basic Function ====================
# Complete warmUpExercise.m 
source("warmUpExercise.R")
message(sprintf('Running warmUpExercise ... \n'))
message(sprintf('5x5 Identity Matrix: \n'))
warmUpExercise()

readline(prompt='Program paused. Press enter to continue.\n')


## ======================= Part 2: Plotting =======================

source("plotData.R")

message(sprintf('Plotting Data ...\n'))
data = read.table('../ex1/ex1data1.txt', sep=",")

#X = data(:, 1); y = data(:, 2);
X = data.matrix(data[, 1])
y = data.matrix(data[, 2])

m = length(y)  # number of training examples

# Plot Data
# Note: You have to complete the code in plotData.m
plotData(X, y)

readline(prompt='Program paused. Press enter to continue.\n')

## =================== Part 3: Gradient descent ===================

source("computeCost.R")
source("gradientDescent.R")

message(sprintf('Running Gradient Descent ...\n'))

#X = [ones(m, 1), data(:,1)]; % Add a column of ones to x

X1 <- cbind(rep(1,m), X)

#theta = zeros(2, 1); % initialize fitting parameters
theta = rep(0, 2)

# Some gradient descent settings
iterations = 1500
alpha = 0.01

# compute and display initial cost
computeCost(X1, y, theta)

# run gradient descent
TandJ = gradientDescent(X1, y, theta, alpha, iterations)
theta <- TandJ$theta

# print theta to screen
message(sprintf('Theta found by gradient descent: '))
message(sprintf('%f %f \n', theta[1], theta[2]))

# Plot the linear fit
#hold on; % keep previous plot visible
par(new=TRUE)
#plot(X(:,2), X*theta, '-')
plot(X, X1 %*% theta, type = "l", col="blue", xlab="", ylab="")
legend("bottomright", c('Training data', 'Linear regression'), pch=c(4, 3), col=c("red", "blue"))

#hold off % don't overlay any more plots on this figure

# Predict values for population sizes of 35,000 and 70,000
predict1 = c(1, 3.5) %*% theta
message(sprintf('For population = 35,000, we predict a profit of %f\n',predict1*10000))

predict2 = c(1, 7) %*% theta
message(sprintf('For population = 70,000, we predict a profit of %f\n', predict2*10000))

readline(prompt='Program paused. Press enter to continue.\n')

## ============= Part 4: Visualizing J(theta_0, theta_1) =============
message(sprintf('Visualizing J(theta_0, theta_1) ...\n'))

# Grid over which we will calculate J
#theta0_vals = linspace(-10, 10, 100);
#theta1_vals = linspace(-1, 4, 100);
theta0_vals = seq(-10, 10, length=100)
theta1_vals = seq(-1, 4, length=100)


# initialize J_vals to a matrix of 0's
#J_vals = zeros(length(theta0_vals), length(theta1_vals));
J_vals = matrix(0, length(theta0_vals), length(theta1_vals))


# Fill out J_vals
for (i in 1:length(theta0_vals)) {
    for (j in 1:length(theta1_vals)) {
        t = c(theta0_vals[i] , theta1_vals[j])    
        J_vals[i,j] = computeCost(X1, y, t)
    }
}


# Because of the way meshgrids work in the surf command, we need to 
# transpose J_vals before calling surf, or else the axes will be flipped
J_vals = t(J_vals)
# Surface plot
#figure;

#xlabel='\theta_0'
#ylabel= '\theta_1'

xlabel = expression(theta[0])
ylabel = expression(theta[1])

jet.colors <- colorRampPalette( c("blue", "green", "yellow", "red") )
nbcol <- 100
color <- jet.colors(nbcol)
#color <- rainbow(nbcol)

nrz <- nrow(J_vals)
ncz <- ncol(J_vals)
zfacet <- J_vals[-1, -1] + J_vals[-1, -ncz] + J_vals[-nrz, -1] + J_vals[-nrz, -ncz]
# Recode facet z-values into color indices
facetcol <- cut(zfacet, nbcol)

#surf(theta0_vals, theta1_vals, J_vals)
persp(theta0_vals, theta1_vals, J_vals, xlab= xlabel, ylab = ylabel, col=color[facetcol], theta=-45,  ticktype="detailed")

readline(prompt='Program paused. Press enter to continue.\n')


library(matlab)
# Contour plot
#figure;
# Plot J_vals as 15 contours spaced logarithmically between 0.01 and 100

xlim = c(-10, 10)
ylim = c(-1, 4)
#xlabel='\theta_0' 
xlabel=expression(theta[0])
#ylabel='\theta_1'
ylable = expression(theta[1])
#contour(theta0_vals, theta1_vals, J_vals, logspace(-2, 3, 20))
contour(theta0_vals, theta1_vals, t(J_vals), xlim=xlim, ylim = ylim, levels=10^seq(-2,3,len=20))

#hold on;
#plot(theta(1), theta(2), 'rx', 'MarkerSize', 10, 'LineWidth', 2);
par(new=TRUE)
plot(theta[1], theta[2], pch=4, col="red", cex=2, lwd=2, xlim=xlim, ylim=ylim)
