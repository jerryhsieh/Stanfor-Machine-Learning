#
#
#
cofiCostFunc <- function (params, Y, R, num_users, num_movies, num_features, lambda) {
#COFICOSTFUNC Collaborative filtering cost function
#   [J, grad] = COFICOSTFUNC(params, Y, R, num_users, num_movies, ...
#   num_features, lambda) returns the cost and gradient for the
#   collaborative filtering problem.
#

# Unfold the U and W matrices from params
    
#    message("now printing params")
#    print(params)
    
    X = matrix(params[1:(num_movies*num_features)], num_movies, num_features)
    Theta = matrix(params[(num_movies*num_features+1):length(params)],num_users, num_features)

#    print(X)
#    print(Theta)
    
    
# You need to return the following values correctly
    J = 0;
    X_grad = matrix(0, nrow(X), ncol(X));
    Theta_grad = matrix(0, nrow(Theta), ncol(Theta));

    J = (1/2) * sum(sum(((X %*% t(Theta) - Y)^2) * R)) + (lambda / 2) * sum(sum(Theta^ 2)) + (lambda / 2) * sum(sum(X^2))
    
    #message(sprintf("now j is %f", J))

    X_grad = ((X %*% t(Theta) - Y) * R) %*% Theta + lambda * X
                
    Theta_grad = t((X %*% t(Theta) - Y) * R) %*% X + lambda * Theta
                                    
    grad = as.vector(c(as.vector(X_grad), as.vector(Theta_grad)))

    JandG = list(J=J, grad=grad)
}