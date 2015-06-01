plotProgresskMeans <- function (X, centroids, previous, idx, K, i) {
#PLOTPROGRESSKMEANS is a helper function that displays the progress of 
#k-Means as it is running. It is intended for use only with 2D data.
#   PLOTPROGRESSKMEANS(X, centroids, previous, idx, K, i) plots the data
#   points with colors assigned to each centroid. With the previous
#   centroids, it also plots a line between the previous locations and
#   current locations of the centroids.
#

# Plot the examples
source("plotDataPoints.R")    
plotDataPoints(X, idx, K)

# Plot the centroids as black x's
points(previous[,1], previous[,2], pch=4, col="red", lwd=3)
points(centroids[,1], centroids[,2], pch=4, lwd = 3)

#print(previous)
#print(centroids)

# Plot the history of the centroids with lines
for (j in 1:nrow(centroids)) {
#    abline(centroids[j, ], previous[j,])
    segments(x0=centroids[j, 1], y0=centroids[j, 2], x1=previous[j,1], y1=previous[j,2])
    #lines(centroids[j, ], previous[j,])
}

# Title
title(sprintf('Iteration number %d', i))

}