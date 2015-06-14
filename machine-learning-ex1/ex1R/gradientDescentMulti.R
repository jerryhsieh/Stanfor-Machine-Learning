gradientDescentMulti <- function (X, y, theta, alpha, num_iters) {
#GRADIENTDESCENTMULTI Performs gradient descent to learn theta
#   theta = GRADIENTDESCENTMULTI(x, y, theta, alpha, num_iters) updates theta by
#   taking num_iters gradient steps with learning rate alpha

# Initialize some useful values
source("computeCostMulti.R")


m = length(y)    # number of training examples
J_history = rep(0, num_iters) #zeros(num_iters, 1)

for (iter in 1:num_iters) {

# ====================== YOUR CODE HERE ======================
# Instructions: Perform a single gradient step on the parameter vector
#               theta. 
#
# Hint: While debugging, it can be useful to print out the values
#       of the cost function (computeCostMulti) and gradient here.
#

    theta = theta - ((1/m) * t(t(X %*% theta - y) %*% X) * alpha)
                 
                 
                 
# ============================================================
                     
# Save the cost J in every iteration    
    J_history[iter] = computeCostMulti(X, y, theta)
                 
}
    TandJ = list(theta = theta , J_history = J_history)
}