plotData <- function (X, y) {
#PLOTDATA Plots the data points X and y into a new figure 
#   PLOTDATA(x,y) plots the data points with + for the positive examples
#   and o for the negative examples. X is assumed to be a Mx2 matrix.
#
# Note: This was slightly modified such that it expects y = 1 or y = 0

# Find Indices of Positive and Negative Examples
pos = find(y == 1) 
neg = find(y == 0)
xlim <- c(min(X[,1]), max(X[,1]))
ylim <- c(min(X[,2]), max(X[,2]))
#xlim <- c(min(X), max(X))
#ylim <- c(min(y), max(y))


# Plot Examples
#plot(X(pos, 1), X(pos, 2), 'k+','LineWidth', 1, 'MarkerSize', 7)
#hold on;
plot(X[pos,1], X[pos, 2], pch=3, cex=1, xlim=xlim, ylim=ylim, xlab="", ylab="")

#plot(X(neg, 1), X(neg, 2), 'ko', 'MarkerFaceColor', 'y', 'MarkerSize', 7)
#hold off;
par(new=TRUE)
plot(X[neg, 1], X[neg,2], pch=21, cex=1, xlim=xlim, ylim=ylim, col="black", bg="yellow", xlab="", ylab="")

}
