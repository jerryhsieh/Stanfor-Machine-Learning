costFunctionReg <- function (theta, X, y, lambda) {
#COSTFUNCTIONREG Compute cost and gradient for logistic regression with regularization
#   J = COSTFUNCTIONREG(theta, X, y, lambda) computes the cost of using
#   theta as the parameter for regularized logistic regression and the
#   gradient of the cost w.r.t. to the parameters. 

# Initialize some useful values
m = length(y)       #  number of training examples

# You need to return the following variables correctly 
J = 0
#grad = zeros(size(theta))

# ====================== YOUR CODE HERE ======================
# Instructions: Compute the cost of a particular choice of theta.
#               You should set J to the cost.
#               Compute the partial derivatives and set grad to the partial
#               derivatives of the cost w.r.t. each parameter in theta

source("sigmoid.R")

theta2 = theta[2:length(theta)]
X2 = matrix(X[,  2:NCOL(X)])

H = sigmoid(X %*% theta)
H2 = sigmoid(X2 %*% theta2)

J = - (1/ m) * (log(t(H)) %*% y + log(t(1 - H)) %*% (1 - y)) + (lambda / (2 * m)) * sum(theta2^ 2)


grad = (1 / m) * t(t(H - y) %*% X) + (lambda / m) * theta
grad[1] = (1 / m) * t(t(H - y) %*% X)[1]
                                       

JandG = list(J = J, grad = grad)

# =============================================================

                                       
}
                                       