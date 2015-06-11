nnCostFunction <- function (nn_params, input_layer_size, hidden_layer_size, num_labels, X, y, lambda) {
#NNCOSTFUNCTION Implements the neural network cost function for a two layer
#neural network which performs classification
#   [J grad] = NNCOSTFUNCTON(nn_params, hidden_layer_size, num_labels, ...
#   X, y, lambda) computes the cost and gradient of the neural network. The
#   parameters for the neural network are "unrolled" into the vector
#   nn_params and need to be converted back into the weight matrices. 
# 
#   The returned parameter grad should be a "unrolled" vector of the
#   partial derivatives of the neural network.
#

# Reshape nn_params back into the parameters Theta1 and Theta2, the weight matrices
# for our 2 layer neural network
Theta1 = matrix(nn_params[1 : (hidden_layer_size * (input_layer_size + 1))], hidden_layer_size, (input_layer_size + 1))

Theta2 = matrix(nn_params[(1 + (hidden_layer_size * (input_layer_size + 1))) :length(nn_params)], num_labels, (hidden_layer_size + 1))

# Setup some useful variables
m = NROW(X)


# You need to return the following variables correctly 
J = 0
Theta1_grad = matrix(0, nrow(Theta1), ncol(Theta1))
Theta2_grad = matrix(0, nrow(Theta2), ncol(Theta2))

# ====================== YOUR CODE HERE ======================
# Instructions: You should complete the code by working through the
#               following parts.
#
# Part 1: Feedforward the neural network and return the cost in the
#         variable J. After implementing Part 1, you can verify that your
#         cost function computation is correct by verifying the cost
#         computed in ex4.m
#
# Part 2: Implement the backpropagation algorithm to compute the gradients
#         Theta1_grad and Theta2_grad. You should return the partial derivatives of
#         the cost function with respect to Theta1 and Theta2 in Theta1_grad and
#         Theta2_grad, respectively. After implementing Part 2, you can check
#         that your implementation is correct by running checkNNGradients
#
#         Note: The vector y passed into the function is a vector of labels
#               containing values from 1..K. You need to map this vector into a 
#               binary vector of 1's and 0's to be used with the neural network
#               cost function.
#
#         Hint: We recommend implementing backpropagation using a for-loop
#               over the training examples if you are implementing it for the 
#               first time.
#
# Part 3: Implement regularization with the cost function and gradients.
#
#         Hint: You can implement this around the code for
#               backpropagation. That is, you can compute the gradients for
#               the regularization separately and then add them to Theta1_grad
#               and Theta2_grad from Part 2.
#


J <- costFunction(nn_params, input_layer_size, hidden_layer_size, num_labels, X, y, lambda)

grad <- gradient(nn_params, input_layer_size, hidden_layer_size, num_labels, X, y, lambda)

JandG = list(J=J, grad=grad)


}

#
# pure cost function, return cost only, for use in optim
#
costFunction <- function(nn_params, input_layer_size, hidden_layer_size, num_labels, X, y, lambda){
    
    Theta1 = matrix(nn_params[1 : (hidden_layer_size * (input_layer_size + 1))], hidden_layer_size, (input_layer_size + 1))
    
    Theta2 = matrix(nn_params[(1 + (hidden_layer_size * (input_layer_size + 1))) :length(nn_params)], num_labels, (hidden_layer_size + 1))
    
    # Setup some useful variables
    m = NROW(X)
    
    fwd <- forward(nn_params, input_layer_size, hidden_layer_size, num_labels, X, y, lambda)
    
    H <- fwd$H
    y_matrix <- fwd$y_matrix
    
    J = - (1/ m) * sum(diag(log(t(H)) %*% y_matrix + log(t(1 - H)) %*% (1 - y_matrix) )) + (lambda / (2 * m)) * (sum(sum(Theta1[,2:ncol(Theta1)]^ 2)) + sum(sum(Theta2[,2:ncol(Theta2)]^2)))
    
}

forward <- function(nn_params, input_layer_size, hidden_layer_size, num_labels, X, y, lambda) {
    
    source("sigmoid.R")
    source("sigmoidGradient.R")
    
    Theta1 = matrix(nn_params[1 : (hidden_layer_size * (input_layer_size + 1))], hidden_layer_size, (input_layer_size + 1))
    
    Theta2 = matrix(nn_params[(1 + (hidden_layer_size * (input_layer_size + 1))) :length(nn_params)], num_labels, (hidden_layer_size + 1))
    
    
    # Setup some useful variables
    m = NROW(X)
    
    A1 = cbind(rep(1, m) ,X)
    Z2 = A1 %*% t(Theta1)
    
    A2 = sigmoid(Z2)
    A2 = cbind(rep(1,m), A2)
    
    Z3 = A2 %*% t(Theta2)
    A3 = sigmoid(Z3)
    
    H = A3
    
    #y_matrix = eye(num_labels)(y, :) # transfer position to vector
    y_matrix = matrix(0, length(y), num_labels)
    yord <- cbind(1:length(y), y)
    y_matrix[yord] <- 1
    
    
    fwd <- list(A1 = A1, A2 = A2, A3 = A3, Z2 = Z2, Z3 = Z3, H = H,  y_matrix  = y_matrix)
}

gradient <- function(nn_params, input_layer_size, hidden_layer_size, num_labels, X, y, lambda) {
    
    Theta1 = matrix(nn_params[1 : (hidden_layer_size * (input_layer_size + 1))], hidden_layer_size, (input_layer_size + 1))
    
    Theta2 = matrix(nn_params[(1 + (hidden_layer_size * (input_layer_size + 1))) :length(nn_params)], num_labels, (hidden_layer_size + 1))
    
    # Setup some useful variables
    m = NROW(X)
    
    fwd <- forward(nn_params, input_layer_size, hidden_layer_size, num_labels, X, y, lambda)
    A3 <- fwd$A3
    Z2 <- fwd$Z2
    A2 <- fwd$A2
    A1 <- fwd$A1
    y_matrix <- fwd$y_matrix
    
# backward    
    D3 = A3 - y_matrix
    D2 = (D3 %*% Theta2[, 2:ncol(Theta2)]) * sigmoidGradient(Z2)
    
    Delta2 = t(D3) %*% A2
    Delta1 = t(D2) %*% A1
    
    Theta2[, 1] = 0
    Theta1[, 1] = 0
    
    Theta2_grad = (1 / m) * Delta2 + (lambda / m) * Theta2
    Theta1_grad = (1 / m) * Delta1 + (lambda / m) * Theta1
    
    # -------------------------------------------------------------
    
    # =========================================================================
    
    # Unroll gradients
    grad = c(as.vector(Theta1_grad), as.vector(Theta2_grad))
    
}
