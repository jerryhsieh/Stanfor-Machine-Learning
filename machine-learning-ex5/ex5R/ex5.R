#
# solve ex5 with R
#
## Machine Learning Online Class
#  Exercise 5 | Regularized Linear Regression and Bias-Variance
#
#  Instructions
#  ------------
# 
#  This file contains code that helps you get started on the
#  exercise. You will need to complete the following functions:
#
#     linearRegCostFunction.m
#     learningCurve.m
#     validationCurve.m
#
#  For this exercise, you will not need to change any code in this file,
#  or any other files other than those mentioned above.
#

# Initialization
rm(list=ls())

#Load library
library(R.matlab)

## =========== Part 1: Loading and Visualizing Data =============
#  We start the exercise by first loading and visualizing the dataset. 
#  The following code will load the dataset into your environment and plot
#  the data.
#

# Load Training Data
message(sprintf('Loading and Visualizing Data ...\n'))


#Read dataset from the matlab file
data <- readMat("../ex5/ex5data1.mat")
X <- data$X
y <- data$y
Xtest <- data$Xtest
ytest <- data$ytest
Xval <- data$Xval
yval <- data$yval


# m = Number of examples
m = NROW(X)

#Plot training data
plot(X,y,xlab="Change in water level (X)",ylab="water flowing out of dam (y)", pch=4, col="red")

readline(prompt='Program paused. Press enter to continue.\n')

## =========== Part 2: Regularized Linear Regression Cost =============
#  You should now implement the cost function for regularized linear 
#  regression. 
#
source("linearRegCostFunction.R")

X1 <- cbind(rep(1,nrow(X)),X) # we will use this X to the following steps
Xval1 <- cbind(rep(1, nrow(Xval)), Xval)

theta <- rep(1,ncol(X1))

JandG = linearRegCostFunction(X1, y, theta, 1)
J <- JandG$J

message(sprintf('Cost at theta = [1 ; 1]: %f ', J))
message(sprintf('\n(this value should be about 303.993192)\n'))

readline(prompt='Program paused. Press enter to continue.\n')


## =========== Part 3: Regularized Linear Regression Gradient =============
#  You should now implement the gradient for regularized linear 
#  regression.
#

source("linearRegCostFunction.R")

#X <- cbind(rep(1,nrow(X)),X)
#theta <- rep(1,ncol(X))

JandG = linearRegCostFunction(X1, y, theta, 1)
grad<- JandG$grad

message(sprintf('Gradient at theta = [1 ; 1]:  [%f; %f] ', grad[1], grad[2]))
message(sprintf('\n(this value should be about [-15.303016; 598.250744])\n'))

readline(prompt='Part 3, Program paused. Press enter to continue.\n')



## =========== Part 4: Train Linear Regression =============
#  Once you have implemented the cost and gradient correctly, the
#  trainLinearReg function will use your cost function to train 
#  regularized linear regression.
# 
#  Write Up Note: The data is non-linear, so this will not give a great 
#                 fit.
#

#  Train linear regression with lambda = 0
#Let us train linear model without regularization and visualize fitted model. By setting #the value of lambda zero will train model without regularization

source("trainLinearReg.R")

lambda = 0
theta_optim = trainLinearReg(X1, y, lambda)

#Plot fitted line
#plot(X,y,xlab="Change in water level (X)",ylab="water flowing out of dam (y)", pch=4, col="red")
abline(coef=theta_optim$par, col="blue")


readline(prompt='Part4 , Program paused. Press enter to continue.\n')


## =========== Part 5: Learning Curve for Linear Regression =============
#  Next, you should implement the learningCurve function. 
#
#  Write Up Note: Since the model is underfitting the data, we expect to
#                 see a graph with "high bias" -- slide 8 in ML-advice.pdf 
#

source("learningCurve.R")

# Error on training and testing
error <- learningCurve(X1, y, Xval1, yval)

# get the range for the x and y axis
xrange <- range(1:nrow(X1))
#yrange <- range(error$error_val)
yrange <- c(0, 150)
colors <- rainbow(2)
linetype <- c(1:2)
plotchar <- seq(18,19,1)

#Learning curve
plot(xrange,yrange,xlab="Number of training example",ylab="Error")
lines(2:nrow(X1), error$error_train[2:nrow(X1)], type="b", lwd=1.5,
      lty=linetype[1], col=colors[1], pch=plotchar[1]) 
lines(2:nrow(X1), error$error_val[2:nrow(X1)], type="b", lwd=1.5,
      lty=linetype[2], col=colors[2], pch=plotchar[2]) 
legend(xrange[1], yrange[2], 1:2, cex=0.8, col=colors,
       pch=plotchar, lty=linetype, title="Linear Regression learing curve")


message(sprintf('# Training Examples\tTrain Error\tCross Validation Error\n'))
for (i in 1:m) {
    message(sprintf('  \t%d\t\t%f\t%f\n', i, error$error_train[i], error$error_val[i]))
}

readline(prompt='Part 5, Program paused. Press enter to continue.\n')


## =========== Part 6: Feature Mapping for Polynomial Regression =============
#  One solution to this is to use polynomial regression. You should now
#  complete polyFeatures to map each example into its powers
#
source("polyFeatures.R")
source("featureNormalize.R")

p = 8

# Map X onto Polynomial Features and Normalize
X_poly = polyFeatures(X, p)
normX <- featureNormalize(X_poly)  # Normalize
X_poly <- normX$X_norm
mu <- normX$mu
sigma <- normX$sigma
X_poly1 = cbind(rep(1, nrow(X_poly)), X_poly)                   # Add Ones


# Map X_poly_test and normalize (using mu and sigma)
X_poly_test <- polyFeatures(Xtest, p)

#X_poly_test = bsxfun(@minus, X_poly_test, mu)
X_poly_test <- sweep(X_poly_test, 2, mu)

#X_poly_test = bsxfun(@rdivide, X_poly_test, sigma)
X_poly_test = sweep(X_poly_test, 2, sigma, "/")

#X_poly_test = [ones(size(X_poly_test, 1), 1), X_poly_test];         # Add Ones
X_poly_test1 <- cbind(rep(1, nrow(X_poly_test)), X_poly_test)


# Map X_poly_val and normalize (using mu and sigma)
X_poly_val = polyFeatures(Xval, p)
#X_poly_val = bsxfun(@minus, X_poly_val, mu);
X_poly_val <- sweep(X_poly_val, 2, mu)

#X_poly_val = bsxfun(@rdivide, X_poly_val, sigma);
X_poly_val = sweep(X_poly_val, 2, sigma, "/")

#X_poly_val = [ones(size(X_poly_val, 1), 1), X_poly_val];           # Add Ones
X_poly_val1 <- cbind(rep(1, nrow(X_poly_val)), X_poly_val)


message(sprintf('Normalized Training Example 1:\n'))
message(sprintf('  %f  \n', X_poly1[1, ]))

readline(prompt='\n Part 6, Program paused. Press enter to continue.\n')



## =========== Part 7: Learning Curve for Polynomial Regression =============
#  Now, you will get to experiment with polynomial regression with multiple
#  values of lambda. The code below runs polynomial regression with 
#  lambda = 0. You should try running the code with different values of
#  lambda to see how the fit and learning curve change.
#

source("plotFit.R")

lambda = 3
theta = trainLinearReg(X_poly1, y, lambda)

theta <- theta$par

# Plot training data and fit
#figure(1);
#plot(X, y, 'rx', 'MarkerSize', 10, 'LineWidth', 1.5);
xrange <- c(-60, 70)
yrange <- c(-150, 500)
plot(X,y,xlim = xrange, ylim = yrange, xlab="Change in water level (X)",ylab="water flowing out of dam (y)", pch=4, col="red")
plotFit(min(X), max(X), mu, sigma, theta, p)

readline(prompt="program pause, press enter to continue\n")


#figure(2);
error = learningCurve(X_poly1, y, X_poly_val1, yval, lambda)
error_train <- error$error_train
error_val <- error$error_val

title = sprintf('Polynomial Regression Learning Curve (lambda = %f)', lambda)
xlabel = 'Number of training examples'
ylabel = 'Error'

#plot(1:m, error_train, 1:m, error_val, main=title, xlab=xlabel, ylab=ylabel)

#axis([0 13 0 100])
#legend('Train', 'Cross Validation')

# get the range for the x and y axis
xrange <- range(1:nrow(X1))
#yrange <- range(error$error_val)
yrange <- c(0, 100)
colors <- rainbow(2)
linetype <- c(1:2)
plotchar <- seq(18,19,1)

#Learning curve
plot(xrange,yrange,xlab=xlabel,ylab=ylabel, main=title)
lines(2:nrow(X1), error_train[2:nrow(X1)], type="b", lwd=1.5,
      lty=linetype[1], col=colors[1], pch=plotchar[1]) 
lines(2:nrow(X1), error_val[2:nrow(X1)], type="b", lwd=1.5,
      lty=linetype[2], col=colors[2], pch=plotchar[2]) 
legend(xrange[1], yrange[2], 1:2, cex=0.8, col=colors,
       pch=plotchar, lty=linetype, title="Polynomial Regression learing curve")


message(sprintf('Polynomial Regression (lambda = %f)\n\n', lambda))
message(sprintf('# Training Examples\tTrain Error\tCross Validation Error\n'))

for (i in 1:m) {
    message(sprintf('  \t%d\t\t%f\t%f\n', i, error_train[i], error_val[i]))
}

readline(prompt='Part 7, Program paused. Press enter to continue.\n')

## =========== Part 8: Validation for Selecting Lambda =============
#  You will now implement validationCurve to test various values of 
#  lambda on a validation set. You will then use this to select the
#  "best" lambda value.
#
source("validationCurve.R")

lerror <- validationCurve(X_poly1, y, X_poly_val1, yval)
lambda_vec <- lerror$lambda_vec
error_train <- lerror$error_train
error_val <- lerror$error_val

#close all;


xlabel='lambda'
ylabel='Error'

# get the range for the x and y axis
xrange <- range(lambda_vec)
#yrange <- range(error$error_val)
yrange <- range(error_val)
colors <- rainbow(2)
linetype <- c(1:2)
plotchar <- seq(18,19,1)

#Learning curve
plot(xrange,yrange,xlab=xlabel,ylab=ylabel)
lines(lambda_vec, error_train, type="b", lwd=1.5,
      lty=linetype[1], col=colors[1], pch=plotchar[1]) 
lines(lambda_vec, error_val, type="b", lwd=1.5,
      lty=linetype[2], col=colors[2], pch=plotchar[2]) 
#legend(xrange[1], yrange[2], 1:2, cex=0.8, col=colors, pch=plotchar, lty=linetype, legend=c("Train", "Cross Validation"))
#legend('Train', 'Cross Validation')
legend(xrange[1], yrange[2], legend=c("Train", "Cross Validation"), pch=plotchar, lty=linetype, col=colors, cex=0.8)
message(sprintf('lambda\t\tTrain Error\tValidation Error\n'))


for (i in 1:length(lambda_vec)) {
    message(sprintf(' %f\t%f\t%f\n', lambda_vec[i], error_train[i], error_val[i]))
}

readline(prompt='Program paused. Press enter to continue.\n')



