runkMeans <- function (X, initial_centroids, max_iters, plot_progress=FALSE) {
#RUNKMEANS runs the K-Means algorithm on data matrix X, where each row of X
#is a single example
#   [centroids, idx] = RUNKMEANS(X, initial_centroids, max_iters, ...
#   plot_progress) runs the K-Means algorithm on data matrix X, where each 
#   row of X is a single example. It uses initial_centroids used as the
#   initial centroids. max_iters specifies the total number of interactions 
#   of K-Means to execute. plot_progress is a true/false flag that 
#   indicates if the function should also plot its progress as the 
#   learning happens. This is set to false by default. runkMeans returns 
#   centroids, a Kxn matrix of the computed centroids and idx, a m x 1 
#   vector of centroid assignments (i.e. each entry in range [1..K])
#

# Set default value for plot progress
# plot_progress = FALSE as default

# Plot the data if we are plotting progress
#if (plot_progress){
#    figure
#    hold on
#}

    
source("plotProgressKMeans.R")
source("findClosetCentroids.R")
source("computeCentroids.R")

# Initialize values
m = nrow(X) 
n = ncol(X)

K = nrow(initial_centroids)

centroids = initial_centroids
previous_centroids = centroids
idx = matrix(0, m, 1)

# Run K-Means
for (i in 1:max_iters) {

# Output progress
    message(sprintf('K-Means iteration %d/%d...\n', i, max_iters))
    #if exist('OCTAVE_VERSION')
    #    fflush(stdout);
    #end

# For each example in X, assign it to the closest centroid
    idx = findClosestCentroids(X, centroids)

# Optionally, plot progress here
    if (plot_progress) {

        plotProgresskMeans(X, centroids, previous_centroids, idx, K, i)
        previous_centroids = centroids
        readline(prompt='Press enter to continue.\n')
    }

# Given the memberships, compute new centroids
    centroids = computeCentroids(X, idx, K)
}


CandI = list(centroids, idx)


# Hold off if we are plotting progress
#if plot_progress
#hold off;
#end

}
