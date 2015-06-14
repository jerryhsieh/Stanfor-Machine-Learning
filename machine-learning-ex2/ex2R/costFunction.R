costFunction <- function (theta, X, y) {
#COSTFUNCTION Compute cost and gradient for logistic regression
#   J = COSTFUNCTION(theta, X, y) computes the cost of using theta as the
#   parameter for logistic regression and the gradient of the cost
#   w.r.t. to the parameters.

source("sigmoid.R")

# Initialize some useful values
m = length(y)     # number of training examples

# You need to return the following variables correctly 
J = 0
grad = zeros(size(theta))

# ====================== YOUR CODE HERE ======================
# Instructions: Compute the cost of a particular choice of theta.
#               You should set J to the cost.
#               Compute the partial derivatives and set grad to the partial
#               derivatives of the cost w.r.t. each parameter in theta
#
# Note: grad should have the same dimensions as theta
#

H = sigmoid(X %*% theta)

J = - (1/ m) * (log(t(H)) %*% y + log(t(1 - H)) %*% (1 - y))

grad = (1 / m) * t((t(H - y) %*% X))
                  
                  
# =============================================================
 
JandG = list(J = J, grad = grad)

}                  