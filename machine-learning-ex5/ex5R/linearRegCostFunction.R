linearRegCostFunction <- function (X, y, theta, lambda) {
#LINEARREGCOSTFUNCTION Compute cost and gradient for regularized linear 
#regression with multiple variables
#   [J, grad] = LINEARREGCOSTFUNCTION(X, y, theta, lambda) computes the 
#   cost of using theta as the parameter for linear regression to fit the 
#   data points in X and y. Returns the cost in J and the gradient in grad

# Initialize some useful values
m = length(y)  # number of training examples

# You need to return the following variables correctly 
J = 0
grad <- rep(0,length(theta))



# ====================== YOUR CODE HERE ======================
# Instructions: Compute the cost and gradient of regularized linear 
#               regression for a particular choice of theta.
#
#               You should set J to the cost and grad to the gradient.
#


theta2 = theta[2:length(theta)]

#message(sprintf("inside linearRegCostFunction X row = %d, X col = %d", NROW(X), NCOL(X)))
#message(sprintf("inside linearRegCostFunction theta row = %d, theta col = %d", NROW(theta), NCOL(theta)))
H = X %*% theta

#message(sprintf("inside linearRegCostFunction H row = %d, H col = %d", NROW(H), NCOL(H)))
J = (1/ (2 * m)) * (sum((H - y)^2)) + (lambda / (2 * m)) * sum(theta2^ 2)

grad = (1 / m) * t((t(H - y) %*% X))
temp = theta
temp[1] = 0
grad = grad + (lambda / m) * temp
                  
#grad(2) = (1 / m) * ((H - y)' * X)' + (lambda / m) * theta;
                                                         
# =========================================================================
                                            
#    grad = grad(:);
 
    JandG = list(J=J, grad=grad)
}

#Cost Function
cost <- function(X,y,theta,lambda)
{
    m <- NROW(X)
    
    residual <- (X %*% theta) - y
    J <- (1/(2*m))*sum((residual)^2) + (lambda/(2*m))* sum((theta[2:length(theta)])^2)
    return(J)
}

#Gradient
gradient <- function(X,y,theta,lambda)
{
    
    m <- NROW(X)
    grad <- rep(0,length(theta))
    
    H = X %*% theta
    grad = (1 / m) * t((t(H - y) %*% X))
    temp = theta
    temp[1] = 0
    grad = grad + (lambda / m) * temp
    
    
#    residual <- (X%*%theta) - y
    
#    grad[1] <- (1/m)* sum((residual)*X[,1])
    
#    for(i in 2:length(theta))
#    {
#        grad[i] <- (1/m)* sum((residual)*X[,i]) + (lambda/m)*theta[i]
#    }
    
    return(grad)
}


                                        