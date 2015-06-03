plotDataPoints <- function (X, idx, K) {
#PLOTDATAPOINTS plots data points in X, coloring them so that those with the same
#index assignments in idx have the same color
#   PLOTDATAPOINTS(X, idx, K) plots data points in X, coloring them so that those 
#   with the same index assignments in idx have the same color

# Create palette
#palette = hsv(K + 1);
#colors = rainbow(nrow(unique(idx)))[z]


#print(colors)
# Plot the data
xlim <- c(min(X[,1]), max(X[,1]))
ylim <- c(min(X[,2]), max(X[,2]))    
plot(X[,1], X[,2], pch=1, xlim=xlim, ylim=ylim, col=rainbow(NROW(unique(idx)))[idx])

}