visualizeBoundaryLinear <- function (X, y, model) {
#VISUALIZEBOUNDARYLINEAR plots a linear decision boundary learned by the
#SVM
#   VISUALIZEBOUNDARYLINEAR(X, y, model) plots a linear decision boundary 
#   learned by the SVM and overlays the data on it
source("plotData.R")
    
w = model$w
b = model$b

#xp = linspace(min(X[,1]), max(X[,1]), 100)
xp <- seq(min(X[,1]), max(X[,1]), length=100)

yp = - (w[1]*xp + b)/w[2]

plotData(X, y)
#hold on;
par(new=TRUE)

xlim <- c(ceil(min(X[,1])), floor(max(X[,1])))
ylim <- c(ceil(min(X[,2])), floor(max(X[,2])))

plot(xp, yp, type="l", col = "blue", xlim=xlim, ylim=ylim)
#plot(xp, yp, '-b'); 
#hold off


}
