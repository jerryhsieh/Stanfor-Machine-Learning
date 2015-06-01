checkCostFunction <- function (lambda=0) {
#CHECKCOSTFUNCTION Creates a collaborative filering problem 
#to check your cost function and gradients
#   CHECKCOSTFUNCTION(lambda) Creates a collaborative filering problem 
#   to check your cost function and gradients, it will output the 
#   analytical gradients produced by your code and the numerical gradients 
#   (computed using computeNumericalGradient). These two gradient 
#   computations should result in very similar values.

    source("computeNumericalGradient.R")
    
# Set lambda
    if (is.null(lambda))
        lambda = 0
    end

##Create small problem
    X_t = matrix(runif(12), nrow=4, ncol=3)
    Theta_t = matrix(runif(15), nrow=5, ncol=3)

# Zap out most entries
    Y = X_t %*% t(Theta_t)           # 4 x 5
    G <- matrix(runif(20), nrow(Y), ncol(Y))
    Y <- replace(Y, which(G > 0.5), 0)
    R = matrix(0, nrow(Y), ncol(Y))
    R <- replace(R, which(Y > 0), 1)

# Run Gradient Checking
    X = matrix(rnorm(12), nrow(X_t), ncol(X_t))
    Theta = matrix(rnorm(15),nrow(Theta_t), ncol(Theta_t))
    num_users = ncol(Y)
    num_movies = nrow(Y)
    num_features = ncol(Theta_t)

    param = list(Y, R, num_users, num_movies, num_features, lambda)
    numgrad = computeNumericalGradient(param, as.vector(c(as.vector(X), as.vector(Theta))))

    JandG = cofiCostFunc(as.vector(c(as.vector(X), as.vector(Theta))),  Y, R, num_users,num_movies, num_features, lambda)
    grad = JandG[[2]]

    print(grad)
    print(numgrad)

    message(sprintf('The above two columns you get should be very similar.\n'))
    message(sprintf('(Left-Your Numerical Gradient, Right-Analytical Gradient)\n\n'))

    diff = (sqrt(sum((numgrad-grad)^2))) / (sqrt(sum((numgrad+grad)^2)))

    message(sprintf('If your backpropagation implementation is correct, then \n'))
    message(sprintf('the relative difference will be small (less than 1e-9). \n'))
    message(sprintf('\nRelative Difference: %g\n', diff))



}