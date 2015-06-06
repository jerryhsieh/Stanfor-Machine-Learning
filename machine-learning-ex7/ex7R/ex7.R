# Machine Learning Online Class
#Exercise 7 | Principle Component Analysis and K-Means Clustering
#
#  Instructions
#  ------------
#
#  This file contains code that helps you get started on the
#  exercise. You will need to complete the following functions:
#
#     pca.m
#     projectData.m
#     recoverData.m
#     computeCentroids.m
#     findClosestCentroids.m
#     kMeansInitCentroids.m
#
#  For this exercise, you will not need to change any code in this file,
#  or any other files other than those mentioned above.
#

## Initialization
rm(list=ls())
resetPar <- function() {
    dev.new()
    op <- par(no.readonly = TRUE)
    dev.off()
    op
}

par(resetPar)

## ================= Part 1: Find Closest Centroids ====================
#  To help you implement K-Means, we have divided the learning algorithm 
#  into two functions -- findClosestCentroids and computeCentroids. In this
#  part, you shoudl complete the code in the findClosestCentroids function. 
#
message(sprintf('Finding closest centroids.\n\n'))

library(R.matlab)
library(matlab)

# Load an example dataset that we will be using
data <- readMat('../ex7/ex7data2.mat')

X <- data$X        #300 x 2 matrix


# Select an initial set of centroids
K <- 3 # 3 Centroids

initial_centroids = matrix(c(3, 3, 6, 2, 8, 5), 3, byrow=TRUE)

# Find the closest centroids for the examples using the
# initial_centroids
source("findClosetCentroids.R")
idx = findClosestCentroids(X, initial_centroids)

message(sprintf('Closest centroids for the first 3 examples: \n'))
message(sprintf(' %d', idx[1:3]))
message(sprintf('\n(the closest centroids should be 1, 3, 2 respectively)\n'))

readline(prompt='Program paused. Press enter to continue.\n')

## ===================== Part 2: Compute Means =========================
#  After implementing the closest centroids function, you should now
#  complete the computeCentroids function.
#
message(sprintf('\nComputing centroids means.\n\n'))

#  Compute means based on the closest centroids found in the previous part.
source("computeCentroids.R")
centroids = computeCentroids(X, idx, K);

message(sprintf('Centroids computed after initial finding of closest centroids: \n'))
print(centroids)
message(sprintf('\n(the centroids should be\n'))
    message(sprintf('   [ 2.428301 3.157924 ]\n'))
    message(sprintf('   [ 5.813503 2.633656 ]\n'))
    message(sprintf('   [ 7.119387 3.616684 ]\n\n'))
                    
readline(prompt='Program paused. Press enter to continue.\n')
                    
                    
                    
## =================== Part 3: K-Means Clustering ======================
#  After you have completed the two functions computeCentroids and
#  findClosestCentroids, you have all the necessary pieces to run the
#  kMeans algorithm. In this part, you will run the K-Means algorithm on
#  the example dataset we have provided. 
#
message(sprintf('\nRunning K-Means clustering on example dataset.\n\n'))
                    
# Load an example dataset

data <- readMat('../ex7/ex7data2.mat')
X <- data$X                    

# Settings for running K-Means
K = 3
max_iters = 10
                    
# For consistency, here we set centroids to specific values
# but in practice you want to generate them automatically, such as by
# settings them to be random examples (as can be seen in
# kMeansInitCentroids).
#initial_centroids = [3 3; 6 2; 8 5];
initial_centroids = matrix(c(3, 3, 6, 2, 8, 5), 3, byrow=TRUE)

# Run K-Means algorithm. The 'true' at the end tells our function to plot
# the progress of K-Means

source("runKMeans.R")
CandI <- runkMeans(X, initial_centroids, max_iters, TRUE);

centroids <- CandI$centroids
idx <- CandI$idx

message(sprintf('\nK-Means Done.\n\n'))
                    
readline(prompt='Program paused. Press enter to continue.\n')
                    
## ============= Part 4: K-Means Clustering on Pixels ===============
#  In this exercise, you will use K-Means to compress an image. To do this,
#  you will first run K-Means on the colors of the pixels in the image and
#  then you will map each pixel on to it's closest centroid.
#  
#  You should now complete the code in kMeansInitCentroids.m
#
                    
message(sprintf('\nRunning K-Means clustering on pixels from an image.\n\n'))
                    
#  Load an image of a bird
#A = double(imread('bird_small.png'))
library(png)
A <- readPNG("../ex7/bird_small.png")

# If imread does not work for you, you can try instead
#   load ('bird_small.mat');
#data <- readMat('../ex7/bird_small.mat')
#A <- data$A




A = A / 255 # Divide by 255 so that all values are in the range 0 - 1
                    
# Size of the image
img_size = dim(A)
                    
# Reshape the image into an Nx3 matrix where N = number of pixels.
#% Each row will contain the Red, Green and Blue pixel values
# This gives us our dataset matrix X that we will use K-Means on.
X = matrix(A, img_size[1] * img_size[2], 3)
                    
# Run your K-Means algorithm on this data
# You should try different values of K and max_iters here
K = 16
max_iters = 10
                    
# When using K-Means, it is important the initialize the centroids
# randomly. 
# You should complete the code in kMeansInitCentroids.m before proceeding
source("kMeansInitCentroids.R")
initial_centroids = kMeansInitCentroids(X, K)
                    
# Run K-Means

CandI <- runkMeans(X, initial_centroids, max_iters)

centroids <- CandI$centroids
idx <- CandI$idx

readline(prompt='Program paused. Press enter to continue.\n')
                    
                    
# ================= Part 5: Image Compression ======================
#  In this part of the exercise, you will use the clusters of K-Means to
#  compress an image. To do this, we first find the closest clusters for
#  each example. After that, we 
                    
message(sprintf('\nApplying K-Means to compress an image.\n\n'))
                    
# Find closest cluster members
idx = findClosestCentroids(X, centroids)
                    
# Essentially, now we have represented the image X as in terms of the
# indices in idx. 
                    
# We can now recover the image from the indices (idx) by mapping each pixel
# (specified by it's index in idx) to the centroid value
X_recovered = centroids[idx,]
                       
# Reshape the recovered image into proper dimensions
X_recovered = array(X_recovered, dim=c(img_size[1], img_size[2], img_size[3]))
                       
# Display the original image 
#subplot(1, 2, 1);
dev.size()           # check device height and width
#par(mfcol=c(1,2))
#imagesc(A)
A <- A * 255
op <- par(no.readonly=TRUE) #store default

plot.new()
plot(c(0, 300), c(0, 600), type = "n", xlab = "", ylab = "", axes=FALSE)
rasterImage(A, 0,0, 150, 500)
text(100, 550, 'Original')
                       
# Display compressed image side by side
#subplot(1, 2, 2);
#imagesc(X_recovered)
X_recovered <- X_recovered * 255

rasterImage(X_recovered, 150,0, 300, 500)
text(220, 550, sprintf('Compressed, with %d colors.', K))
                       
#par(mfcol=c(1,1))
                       
par(op)

readline(prompt='Program paused. Press enter to continue.\n')

