## Machine Learning Online Class
#  Exercise 7 | Principle Component Analysis and K-Means Clustering
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
#clear ; close all; clc
rm(list=ls())



## ================== Part 1: Load Example Dataset  ===================
#  We start this exercise by using a small dataset that is easily to
#  visualize
#
message(sprintf('Visualizing example dataset for PCA.\n\n'))

#  The following command loads the dataset. You should now have the 
#  variable X in your environment
library(R.matlab)
library(matlab)
data <- readMat ('../ex7/ex7data1.mat')

X <- data$X

#  Visualize the example dataset
plot(X[, 1], X[, 2])
#axis([0.5 6.5 2 8])

readline(prompt='Program paused. Press enter to continue.\n')


## =============== Part 2: Principal Component Analysis ===============
#  You should now implement PCA, a dimension reduction technique. You
#  should complete the code in pca.m
#
message(sprintf('\nRunning PCA on example dataset.\n\n'))

#  Before running PCA, it is important to first normalize X
source("featureNormalize.R")
normX= featureNormalize(X)

X_norm <- normX[[1]]
mu <- normX[[2]]
sigma<- normX[[3]]

#  Run PCA
source("pca.R")
USV <- pca(X_norm)
U <- USV$u
S <- diag(USV$d)


#  Compute mu, the mean of the each feature

#  Draw the eigenvectors centered at mean of data. These lines show the
#  directions of maximum variations in the dataset.
#hold on;
a <- mu
b <- mu + 1.5 * S[1,1] * t(U[,1])
segments(a[1],a[2],b[1],b[2], lwd=2)

c<- mu + 1.5 * S[2,2] * t(U[,2])
segments(a[1],a[2],c[1],c[2], lwd=2)
#hold off;

message(sprintf('Top eigenvector: \n'))
message(sprintf(' U(:,1) = %f %f \n', U[1,1], U[2,1]))
message(sprintf('\n(you should expect to see -0.707107 -0.707107)\n'))

readline(prompt='Program paused. Press enter to continue.\n')


## =================== Part 3: Dimension Reduction ===================
#  You should now implement the projection step to map the data onto the 
#  first k eigenvectors. The code will then plot the data in this reduced 
#  dimensional space.  This will show you what the data looks like when 
#  using only the corresponding eigenvectors to reconstruct it.
#
#  You should complete the code in projectData.m
#
message(sprintf('\nDimension reduction on example dataset.\n\n'))

op <- par(no.readonly = TRUE)

#  Plot the normalized dataset (returned from pca)
xlim=c(max(X_norm[,1])+0.5, min(X_norm[,1]) -0.5)
ylim=c(max(X_norm[,2])+0.5, min(X_norm[,2])- 0.5)
plot(X_norm[, 1], X_norm[, 2], pch=1, col="blue", xlim=xlim, ylim=ylim,xlab = "",ylab="")
#axis([-4 3 -4 3]); axis square

#  Project the data onto K = 1 dimension
source("projectData.R")
K = 1
Z = projectData(X_norm, U, K)

message(sprintf('Projection of the first example: %f\n', Z[1]))
message(sprintf('\n(this value should be about 1.481274)\n\n'))

source("recoverData.R")
X_rec  = recoverData(Z, U, K)
message(sprintf('Approximation of the first example: %f %f\n', X_rec[1, 1], X_rec[1, 2]))
message(sprintf('\n(this value should be about  -1.047419 -1.047419)\n\n'))

#  Draw lines connecting the projected points to the original points
#hold on;
par(new = TRUE)
plot(X_rec[, 1], X_rec[, 2], pch=1, col="red", xlim=xlim,ylim=ylim)
for (i in 1:nrow(X_norm)) {
#    drawLine(X_norm(i,:), X_rec(i,:), '--k', 'LineWidth', 1);
        a <- X_norm[i,]
        b <- X_rec[i, ]
        segments(a[1], a[2], b[1], b[2])
}
#hold off

par(op)
readline(prompt='Program paused. Press enter to continue.\n')

## =============== Part 4: Loading and Visualizing Face Data =============
#  We start the exercise by first loading and visualizing the dataset.
#  The following code will load the dataset into your environment
#
message(sprintf('\nLoading face dataset.\n\n'))

#  Load Face dataset
data <- readMat ('../ex7/ex7faces.mat')

X <- data$X

#  Display the first 100 faces in the dataset
source("displayData.R")
displayData(X[1:100, ])

readline(prompt='Program paused. Press enter to continue.\n')

## =========== Part 5: PCA on Face Data: Eigenfaces  ===================
#  Run PCA and visualize the eigenvectors which are in this case eigenfaces
#  We display the first 36 eigenfaces.
#
message(sprintf('\nRunning PCA on face dataset.\n'))
message(sprintf('(this mght take a minute or two ...)\n\n'))

#  Before running PCA, it is important to first normalize X by subtracting 
#  the mean value from each feature
 
normX <- featureNormalize(X)

X_norm <- normX[[1]]
mu <- normX[[2]]
sigma <- normX[[3]]

#  Run PCA

USV <- pca(X_norm)
U <- USV$u
S <- diag(USV$d)


#  Visualize the top 36 eigenvectors found

displayData(t(U[, 1:36]))
            
message(sprintf('Program paused. Press enter to continue.\n'))
            
            
## ============= Part 6: Dimension Reduction for Faces =================
#  Project images to the eigen space using the top k eigenvectors 
#  If you are applying a machine learning algorithm 
            
message(sprintf('\nDimension reduction for face dataset.\n\n'))
            
K = 100
Z <- projectData(X_norm, U, K)
            
message(sprintf('The projected data Z has a size of: '))
message(sprintf('%d ', size(Z)))
            
readline(prompt='\n\nProgram paused. Press enter to continue.\n')
            
## ==== Part 7: Visualization of Faces after PCA Dimension Reduction ====
#  Project images to the eigen space using the top K eigen vectors and 
#  visualize only using those K dimensions
#  Compare to the original input, which is also displayed
            
message(sprintf('\nVisualizing the projected (reduced dimension) faces.\n\n'))
            
K = 100
X_rec  = recoverData(Z, U, K)
            
# Display normalized data
#subplot(1, 2, 1);
par(mfrow=c(1,2))
displayData(X_norm[1:100,])
title('Original faces')
#axis square;
            
# Display reconstructed data from only k eigenfaces
#subplot(1, 2, 2);
displayData(X_rec[1:100,])
title('Recovered faces')
#axis square

par(mfrow=c(1,1))

readline(prompt='Program paused. Press enter to continue.\n')
            
            
## === Part 8(a): Optional (ungraded) Exercise: PCA for Visualization ===
#  One useful application of PCA is to use it to visualize high-dimensional
#  data. In the last K-Means exercise you ran K-Means on 3-dimensional 
#  pixel colors of an image. We first visualize this output in 3D, and then
#  apply PCA to obtain a visualization in 2D.
            
#close all; close all; clc
 
rm(list=ls())

# Re-load the image from the previous exercise and run K-Means on it
# For this to work, you need to complete the K-Means assignment first
            
#A = double(imread('bird_small.png'));
library(png)
A <- readPNG("../ex7/bird_small.png")


# If imread does not work for you, you can try instead
#   load ('bird_small.mat');
            
A = A / 255
            
img_size = dim(A)
X = matrix(A, img_size[1] * img_size[2], 3)

K <- 16
max_iters <- 10
source("kMeansInitCentroids.R")
source("runKMeans.R")

initial_centroids = kMeansInitCentroids(X, K)

CandI= runkMeans(X, initial_centroids, max_iters)
     
centroids <- CandI[[1]]
idx <- CandI[[2]]

#  Sample 1000 random indexes (since working with all the data is
#  too expensive. If you have a fast computer, you may increase this.
        
sel = floor(runif(1000) * nrow(X)) + 1
            
#  Setup Color Palette
#palette = hsv(K);
#colors = palette(idx(sel), :);

#  Visualize the data and centroid memberships in 3D
#figure;
library(scatterplot3d)
scatterplot3d(X[sel, 1], X[sel, 2], X[sel, 3], pch=16, color=rainbow(length(unique(idx[sel])))[idx[sel]])
title('Pixel dataset plotted in 3D. Color shows centroid memberships');
message(sprintf('Program paused. Press enter to continue.\n'))

            
## === Part 8(b): Optional (ungraded) Exercise: PCA for Visualization ===
# Use PCA to project this cloud to 2D for visualization
            
# Subtract the mean to use PCA
source("featureNormalize.R")
source("projectData.R")
source("pca.R")
source("plotDataPoints.R")


normX= featureNormalize(X)

X_norm = normX[[1]]
mu <- normX[[2]]
sigma <- normX[[3]]

            
# PCA and project the data to 2D

USV <- pca(X_norm)

S <- diag(USV$d)
U <- USV$u

Z = projectData(X_norm, U, 2)
            
# Plot in 2D
#figure;
plotDataPoints(Z[sel, ], idx[sel], K)
title('Pixel dataset plotted in 2D, using PCA for dimensionality reduction')
readline(prompt='Program paused. Press enter to continue.\n')
