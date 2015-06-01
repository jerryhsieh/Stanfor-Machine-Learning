cost <- function(X, y, theta) {
  sum( (X %*% theta - y)^2 ) / (2*length(y))
}

# learning rate and iteration limit

grad <- function(X, y, theta, alpha, num_iters) {

	# keep history
	m <- nrow(X)
	n <- ncol(X)
	cost_history <- double(num_iters)
	theta_history <- list(num_iters)

	# initialize coefficients
	#theta <- matrix(0, n, 1)

	# add a column of 1's for the intercept coefficient
	X <- cbind(rep(1,m), matrix(X))

	##debug print(dim(X))
	##debug print(dim(theta))
	# gradient descent
	for (i in 1:num_iters) {
  		error <- (X %*% theta - y)
  		delta <- t(X) %*% error / length(y)
  		theta <- theta - alpha * delta
  		cost_history[i] <- cost(X, y, theta)
  		theta_history[[i]] <- theta
	}
	a <- list(theta_history,theta)
	return(a)
}