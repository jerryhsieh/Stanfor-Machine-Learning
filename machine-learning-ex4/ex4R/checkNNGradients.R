checkNNGradients <- function (lambda) {
#CHECKNNGRADIENTS Creates a small neural network to check the
#backpropagation gradients
#   CHECKNNGRADIENTS(lambda) Creates a small neural network to check the
#   backpropagation gradients, it will output the analytical gradients
#   produced by your backprop code and the numerical gradients (computed
#   using computeNumericalGradient). These two gradient computations should
#   result in very similar values.
#

source("debugInitializeWeights.R")    
source("computeNumericalGradient.R")
#if ~exist('lambda', 'var') || isempty(lambda)
#lambda = 0;
#end
if (missing(lambda)) {
    
    lambda <- 0
}
    
    
input_layer_size <- 3
hidden_layer_size <- 5
num_labels <- 3
m <- 5

# We generate some 'random' test data
Theta1 = debugInitializeWeights(hidden_layer_size, input_layer_size)
Theta2 = debugInitializeWeights(num_labels, hidden_layer_size)
# Reusing debugInitializeWeights to generate X
X  = debugInitializeWeights(m, input_layer_size - 1)
#y  = 1 + mod(1:m, num_labels)'
y = 1 + c(1:m)%%num_labels


# Unroll parameters
nn_params = c(as.vector(Theta1), as.vector(Theta2))

# Short hand for cost function
JandG = nnCostFunction(nn_params, input_layer_size, hidden_layer_size, num_labels, X, y, lambda)
J <- JandG$J
grad <- JandG$grad

params <- list(input_layer_size=input_layer_size, hidden_layer_size = hidden_layer_size, num_labels = num_labels, X=X, y=y, lambda=lambda)
numgrad = computeNumericalGradient(params, nn_params)

# Visually examine the two gradient computations.  The two columns
# you get should be very similar. 
#disp([numgrad grad])
cat(sprintf("%10.5f\t%10.5f \n", numgrad, grad))

message(sprintf('The above two columns you get should be very similar.\n'))
message(sprintf('(Left-Your Numerical Gradient, Right-Analytical Gradient)\n\n'))

# Evaluate the norm of the difference between two solutions.  
# If you have a correct implementation, and assuming you used EPSILON = 0.0001 
# in computeNumericalGradient.m, then diff below should be less than 1e-9
diff = norm(numgrad-grad)/norm(numgrad+grad)
#diff = (sqrt(sum((numgrad-grad)^2))) / (sqrt(sum((numgrad+grad)^2)))


message(sprintf('If your backpropagation implementation is correct, then \n'))
message(sprintf('the relative difference will be small (less than 1e-9). \n'))
message(sprintf('\nRelative Difference: %g\n', diff))

}
