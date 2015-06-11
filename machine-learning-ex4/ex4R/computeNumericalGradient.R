computeNumericalGradient <- function (params, theta) {
#COMPUTENUMERICALGRADIENT Computes the gradient using "finite differences"
#and gives us a numerical estimate of the gradient.
#   numgrad = COMPUTENUMERICALGRADIENT(J, theta) computes the numerical
#   gradient of the function J around theta. Calling y = J(theta) should
#   return the function value at theta.

# Notes: The following code implements numerical gradient checking, and 
#        returns the numerical gradient.It sets numgrad(i) to (a numerical 
#        approximation of) the partial derivative of J with respect to the 
#        i-th input argument, evaluated at theta. (i.e., numgrad(i) should 
#        be the (approximately) the partial derivative of J with respect 
#        to theta(i).)
#                
source("nnCostFunction.R")
    
numgrad = zeros(size(theta))
perturb = zeros(size(theta))
e = 1e-4
input_layer_size <- params$input_layer_size
hidden_layer_size <- params$hidden_layer_size
num_labels <- params$num_labels
X <- params$X
y <- params$y
lambda <- params$lambda


for (p in 1:numel(theta)) {
# Set perturbation vector
    perturb[p] = e
    loss1 = nnCostFunction((theta - perturb), input_layer_size, hidden_layer_size, num_labels, X, y, lambda)[[1]]
    loss2 = nnCostFunction((theta + perturb), input_layer_size, hidden_layer_size, num_labels, X, y, lambda)[[1]]
# Compute Numerical Gradient
    numgrad[p] = (loss2 - loss1) / (2*e)
    perturb[p] = 0
}

return(numgrad)
}
