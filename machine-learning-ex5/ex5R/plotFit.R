plotFit <- function (min_x, max_x, mu, sigma, theta, p) {
#PLOTFIT Plots a learned polynomial regression fit over an existing figure.
#Also works with linear regression.
#   PLOTFIT(min_x, max_x, mu, sigma, theta, p) plots the learned polynomial
#   fit with power p and feature normalization (mu, sigma).

# Hold on to the current figure
#hold on;

# We plot a range slightly bigger than the min and max values to get
# an idea of how the fit will vary outside the range of the data points
#x = t(min_x - 15: 0.05 : max_x + 25)
x = seq(min_x -15, max_x+25, by=0.05)

# Map the X values 
X_poly = polyFeatures(x, p)
#X_poly = bsxfun(@minus, X_poly, mu);
#X_poly = bsxfun(@rdivide, X_poly, sigma);

X_poly = sweep(X_poly, 2, mu)
X_poly = sweep(X_poly, 2, sigma, "/")


# Add ones
#X_poly = [ones(size(x, 1), 1) X_poly];
X_poly1 = cbind(rep(1, nrow(X_poly)), X_poly)                   # Add Ones

# Plot
yps <- X_poly1 %*% theta
points(x, yps, type="l", lwd= 2, col="blue")


print(theta)
# Hold off to the current figure
#hold off

}


#xlabel='Change in water level (x)'
#ylabel='Water flowing out of the dam (y)'
#title = sprintf('Polynomial Regression Fit (lambda = %f)', lambda)

#plot(X, y, pch = 4, col="red", lwd=1.5 , xlab=xlabel, ylab= ylabel, main=title)

#Xs <- matrix(sort(X))                   # plot lines need decreasing X
#Xps1 <- X_poly1[order(X_poly1[,2]), ]   # poly X with sorted X1 (not X0)
#yps <- Xps1 %*% theta                   # predict y

#lines(Xs, yps, col='blue', type='b') 



