#
#
#
visualizeFit <- function(X, mu, sigma2) {

    source("multivariateGaussian.R")
    
	a <- seq(0, 35, by=0.5)
	X1 <- matrix(rep(a, length(a)), length(a), byrow=TRUE)
    X2 <- matrix(rep(a, length(a)), length(a))

	X3 <- cbind(as.vector(X1), as.vector(X2))

	Z <- multivariateGaussian(X3, mu, sigma2)
	readline(prompt="press enter to continue\n")

	Z <- matrix(Z, nrow=length(a))
    
    #	contour(list(x =as.vector(X1), y=as.vector(X2), z=Z))
    xlim <- c(min(X[,1]), max(X[,1]))
    ylim <- c(min(X[,2]), max(X[,2]))
    par(new=TRUE)
    contour(a, a, t(Z), levels=10^seq(-20, 0, by=3))
}

 
