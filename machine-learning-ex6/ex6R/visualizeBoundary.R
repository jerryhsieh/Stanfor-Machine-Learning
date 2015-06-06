visualizeBoundary <- function (X, y, model, varargin) {
#VISUALIZEBOUNDARY plots a non-linear decision boundary learned by the SVM
#   VISUALIZEBOUNDARYLINEAR(X, y, model) plots a non-linear decision 
#   boundary learned by the SVM and overlays the data on it
library(matlab)
source("plotData.R")
source("svmPredict.R")

# Plot the training data on top of the boundary
plotData(X, y)

# Make classification predictions over a grid of values
#x1plot = linspace(min(X(:,1)), max(X(:,1)), 100)'
#x2plot = linspace(min(X(:,2)), max(X(:,2)), 100)'
x1plot = seq(min(X[,1]), max(X[,1]), length=100)
x2plot = seq(min(X[,2]), max(X[,2]), length=100)


X12 <- meshgrid(x1plot, x2plot)
X1 <- X12$x
X2 <- X12$y

vals = matrix(0, nrow(X1), ncol(X1))

for (i in 1:ncol(X1)) {
    this_X = cbind(X1[, i], X2[, i])
    vals[, i] = svmPredict(model, this_X)
}

# Plot the SVM boundary
#hold on
#contour(X1, X2, vals, [0 0], 'Color', 'b');
#contour(X1, X2, vals, [0.0001 0.0001], col="blue")
par(new=TRUE)
contour(x1plot, x2plot, t(vals), col="blue")

#hold off;

}
