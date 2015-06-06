gradientDescent <- function(Y, R, init_X, init_theta, alpha, num_iters) {
    
#GRADIENTDESCENT Performs gradient descent to learn theta
#   theta = GRADIENTDESENT(X, y, theta, alpha, num_iters) updates theta by 
#   taking num_iters gradient steps with learning rate alpha
    
# Initialize some useful values
#    m = length(y) # number of training examples
    J_history = rep(0, num_iters)
    
    num_users <- ncol(Y)
    num_movies <- nrow(Y)
    num_features <- ncol(init_theta)
    
    X <- init_X
    Theta <- init_theta
    
    message(sprintf("got alpha = %f\n",alpha))
    
    for (iter in 1:num_iters) {
        
        X = X - alpha * (((X %*% t(Theta) - Y) * R) %*% Theta + lambda * X)
        Theta = Theta - alpha * (t((X %*% t(Theta) - Y) * R) %*% X + lambda * Theta)

        print(iter)
        
    # Save the cost J in every iteration
        J = (1/2) * sum(sum(((X %*% t(Theta) - Y)^2) * R)) + (lambda / 2) * sum(sum(Theta^ 2)) + (lambda / 2) * sum(sum(X^2))
        if (is.nan(J)) stop("J is too large, try smaller alpha")
        J_history[iter] = J
        message(sprintf("Iter = %4i Got J = %4.6e", iter, J_history[iter]))
    }
    
    message(sprintf("\n"))
    
    plot(J_history)
    XandTheta <- as.vector(c(as.vector(X), as.vector(Theta)))
}