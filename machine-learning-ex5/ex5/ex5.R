#
# solve ex5 with R
#

#Load library
library(R.matlab)

#Read dataset from the matlab file
data <- readMat("ex5data1.mat")

#Plot training data
plot(data$X,data$y,xlab="Change in water level (X)",ylab="water flowing out of dam (y)")

#Predictor variables
X <- as.matrix(data$X)

#Add ones to X
X <- cbind(rep(1,nrow(X)),X)

#Response variable
y <- as.matrix(data$y)

#Cost Function
cost <- function(X,y,theta,lambda)
{
  m <- nrow(X)
  residual <- (X%*%theta) - y
  J <- (1/(2*m))*sum((residual)^2)+(lambda/(2*m))* sum((theta[2:length(theta)])^2)
  return(J)
}


#initial theta
theta <- rep(1,ncol(X))

#inital lambda
lambda <- 1

#cost at inital theta
cost(X,y,theta,lambda)

#Gradient
gradient <- function(X,y,theta,lambda)
{
  m <- nrow(X)
  grad <- rep(0,length(theta))
  residual <- (X%*%theta) - y
  
  grad[1] <- (1/m)* sum((residual)*X[,1])
  
  for(i in 2:length(theta))
  {
    grad[i] <- (1/m)* sum((residual)*X[,i]) + (lambda/m)*theta[i]
  }
  
  return(grad)
}

#Gradient at initial theta
gradient(X,y,theta,lambda)

#Let us train linear model without regularization and visualize fitted model. By setting #the value of lambda zero will train model without regularization

#Set inital theta for training the linear regression
initial_theta <- rep(0,ncol(X))

#Set lambda
lambda <- 0

# Derive theta using gradient descent using optim function
theta_optim <- optim(par=initial_theta,fn=cost,X=X,y=y,lambda=lambda)

#Plot fitted line
plot(data$X,data$y,xlab="Change in water level (X)",ylab="water flowing out of dam (y)")
abline(coef=theta_optim$par)

#cross validation set
#X_val
X_val <- as.matrix(data$Xval)

#add ones to X_val
X_val <- cbind(rep(1,nrow(X_val)),X_val)

#y_val
y_val <- as.matrix(data$yval)


#Learning curve
leaningCurve <- function(X,y,X_val,y_val)
{
  m <- nrow(X)
  error_train <- rep(0,m)
  error_val <- rep(0,m)
  
  for(i in 2:m)
  {
    initial_theta <- rep(1,ncol(X))
    optim <- optim(par=initial_theta,fn=cost,X=X[1:i,],y=y[1:i,],lambda=0)
    theta <- optim$par
    error_train[i] <- cost(X=X[1:i,],y=y[1:i,],theta,lambda=0)
    error_val[i] <- cost(X=X_val,y=y_val,theta,lambda=0)
  }
  
  return(list(error_train=error_train,error_val=error_val))

}

# Error on training and testing
error <- leaningCurve(X,y,X_val,y_val)

# get the range for the x and y axis
xrange <- range(1:nrow(X))
yrange <- range(error$error_val)
colors <- rainbow(2)
linetype <- c(1:2)
plotchar <- seq(18,19,1)

#Learning curve
plot(xrange,yrange,xlab="Number of training example",ylab="Error")
lines(2:nrow(X), error$error_train[2:nrow(X)], type="b", lwd=1.5,
      lty=linetype[1], col=colors[1], pch=plotchar[1]) 
lines(2:nrow(X), error$error_val[2:nrow(X)], type="b", lwd=1.5,
      lty=linetype[2], col=colors[2], pch=plotchar[2]) 
legend(xrange[1], yrange[2], 1:2, cex=0.8, col=colors,
       pch=plotchar, lty=linetype, title="Linear Regression learing curve")



