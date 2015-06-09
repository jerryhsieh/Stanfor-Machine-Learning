trainLinearReg <- function(X, y, lambda) {
#TRAINLINEARREG Trains linear regression given a dataset (X, y) and a
#regularization parameter lambda
#   [theta] = TRAINLINEARREG (X, y, lambda) trains linear regression using
#   the dataset (X, y) and regularization parameter lambda. Returns the
#   trained parameters theta.
#
library(matlab)
source("linearRegCostFunction.R")
    
# Initialize Theta
#initial_theta = zeros(size(X, 2), 1); 
initial_theta <- rep(0,size(X, 2))

# Create "short hand" for the cost function to be minimized
#costFunction = linearRegCostFunction(X, y, t, lambda)

# Now, costFunction is a function that takes in only one argument
#options = optimset('MaxIter', 200, 'GradObj', 'on');

# Minimize using fmincg
#theta = fmincg(costFunction, initial_theta, options);

# Derive theta using gradient descent using optim function

theta_optim <- optim(par=initial_theta,fn=cost, gr = gradient, X=X, y=y,lambda=lambda, method=c("L-BFGS-B"))

#
return(theta_optim)

}
