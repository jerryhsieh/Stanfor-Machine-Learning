# =============== Part 1: Loading movie ratings dataset ================
#  You will start by loading the movie ratings dataset to understand the
#  structure of the data.
#  

rm(list=ls())

library(R.matlab)
library(matlab)

message(sprintf('Loading movie ratings dataset.\n\n'))

#  Load data
data <- readMat('../ex8/ex8_movies.mat')

Y <- data$Y
R <- data$R

#  Y is a 1682x943 matrix, containing ratings (1-5) of 1682 movies on 
#  943 users
#
#  R is a 1682x943 matrix, where R(i,j) = 1 if and only if user j gave a
#  rating to movie i

#  From the matrix, we can compute statistics like average rating.
message(sprintf('Average rating for movie 1 (Toy Story): %f / 5\n\n',mean(Y[1, which(R[1,] > 0)])))

#  We can "visualize" the ratings matrix by plotting it with imagesc

imagesc(Y, ylim=c(nrow(Y), 0))


readline(prompt="program pause, press enter to continue")

# ============ Part 2: Collaborative Filtering Cost Function ===========
#  You will now implement the cost function for collaborative filtering.
#  To help you debug your cost function, we have included set of weights
#  that we trained on that. Specifically, you should complete the code in 
#  cofiCostFunc.m to return J.

#  Load pre-trained weights (X, Theta, num_users, num_movies, num_features)
data <- readMat ('../ex8/ex8_movieParams.mat')

X <- data$X
Theta <- data$Theta
num_users <- data$num.users
num_movies <- data$num.movies
num_features <- data$num.features

#  Reduce the data set size so that this runs faster
num_users = 4 
num_movies = 5
num_features = 3
X = X[1:num_movies, 1:num_features]
Theta = Theta[1:num_users, 1:num_features]
Y = Y[1:num_movies, 1:num_users]
R = R[1:num_movies, 1:num_users]

#  Evaluate cost function
source("cofiCostFunc.R")
JandG = cofiCostFunc(as.vector(c(as.vector(X),as.vector(Theta))), Y, R, num_users, num_movies,num_features, 0)

J <- JandG$J

message(sprintf('Cost at loaded parameters: %f \n(this value should be about 22.22)\n', J))

readline(prompt="program pause, press enter to continue")


## ============== Part 3: Collaborative Filtering Gradient ==============
#  Once your cost function matches up with ours, you should now implement 
#  the collaborative filtering gradient function. Specifically, you should 
#  complete the code in cofiCostFunc.m to return the grad argument.
#  
message(sprintf('\nChecking Gradients (without regularization) ... \n'))

#  Check gradients by running checkNNGradients
source("checkCostFunction.R")
checkCostFunction()

readline(prompt="program pause, press enter to continue")



## ========= Part 4: Collaborative Filtering Cost Regularization ========
#  Now, you should implement regularization for the cost function for 
#  collaborative filtering. You can implement it by adding the cost of
#  regularization to the original cost computation.
#  

#  Evaluate cost function
JandG = cofiCostFunc(as.vector(c(as.vector(X), as.vector(Theta))), Y, R, num_users, num_movies, num_features, 1.5)

message(sprintf('Cost at loaded parameters (lambda = 1.5): %f ', JandG$J))
message('\n(this value should be about 31.34)\n')

message(sprintf('\nProgram paused. Press enter to continue.\n'))


## ======= Part 5: Collaborative Filtering Gradient Regularization ======
#  Once your cost matches up with ours, you should proceed to implement 
#  regularization for the gradient. 
#

#  
message(sprintf('\nChecking Gradients (with regularization) ... \n'))

#  Check gradients by running checkNNGradients
checkCostFunction(1.5)

message(sprintf('\nProgram paused. Press enter to continue.\n'))


## ============== Part 6: Entering ratings for a new user ===============
#  Before we will train the collaborative filtering model, we will first
#  add ratings that correspond to a new user that we just observed. This
#  part of the code will also allow you to put in your own ratings for the
#  movies in our dataset!
#
source("loadMovieList.R")
movieList = loadMovieList();

#  Initialize my ratings
my_ratings = rep(0, length(movieList))

# Check the file movie_idx.txt for id of each movie in our dataset
# For example, Toy Story (1995) has ID 1, so to rate it "4", you can set
my_ratings[1] = 4

# Or suppose did not enjoy Silence of the Lambs (1991), you can set
my_ratings[98] = 2

# We have selected a few movies we liked / did not like and the ratings we
# gave are as follows:
my_ratings[7] = 3
my_ratings[12]= 5
my_ratings[54] = 4
my_ratings[64]= 5
my_ratings[66]= 3
my_ratings[69] = 5
my_ratings[183] = 4
my_ratings[226] = 5
my_ratings[355]= 5


message(sprintf('\n\nNew user ratings:\n'))
for (i in 1:length(my_ratings)) {
    if (my_ratings[i] > 0) 
    message(sprintf('Rated %d for %s\n', my_ratings[i], movieList[i]))
}

readline(prompt='\nProgram paused. Press enter to continue.\n')



## ================== Part 7: Learning Movie Ratings ====================
#  Now, you will train the collaborative filtering model on a movie rating 
#  dataset of 1682 movies and 943 users
#

message(sprintf('\nTraining collaborative filtering...\n'))

#  Load data
data <- readMat('../ex8/ex8_movies.mat')

Y <- data$Y
R <- data$R

#  Y is a 1682x943 matrix, containing ratings (1-5) of 1682 movies by 
#  943 users
#
#  R is a 1682x943 matrix, where R(i,j) = 1 if and only if user j gave a
#  rating to movie i

#  Add our own ratings to the data matrix
Y = cbind(my_ratings, Y)
R = cbind((replace(my_ratings,which(my_ratings > 0), 1)), R)


#  Normalize Ratings
source("normalizeRatings.R")
MandN= normalizeRatings(Y, R)
Ynorm = MandN$Ynorm
Ymean = MandN$Ymean

#  Useful Values
num_users = ncol(Y)
num_movies = nrow(Y)
num_features = 10

# Set Initial Parameters (Theta, X)
init_X = matrix(rnorm(num_movies * num_features), num_movies, num_features)
init_Theta = matrix(rnorm(num_users * num_features) , num_users, num_features)

initial_parameters = as.vector(c(as.vector(init_X), as.vector(init_Theta)))

# Set options for fmincg
#options = optimset('GradObj', 'on', 'MaxIter', 100)

# Set Regularization
lambda = 10
#source("gradientDescent.R")
#theta = fmincg (@(t)(cofiCostFunc(t, Y, R, num_users, num_movies,num_features, lambda)),initial_parameters, options)
alpha <- 0.001
num_iters <- 100
#theta <- gradientDescent(Y, R, init_X, init_Theta, alpha, num_iters)

source("fmincg.R")
theta = fmincg (cofiCostFunc, initial_parameters, Maxiter = num_iters, Y, R, num_users, num_movies,num_features, lambda)
theta <- theta$par

# Unfold the returned theta back into U and W
X = matrix(theta[1:(num_movies*num_features)], num_movies, num_features)
Theta = matrix(theta[(num_movies*num_features+1):length(theta)], num_users, num_features)

message(sprintf('Recommender system learning completed.\n'))

readline(prompt='\nProgram paused. Press enter to continue.\n')

## ================== Part 8: Recommendation for you ====================
#  After training the model, you can now make recommendations by computing
#  the predictions matrix.
#

p = X %*% t(Theta)
my_predictions = p[,1] + Ymean

movieList = loadMovieList();

ix <- order(my_predictions, decreasing = TRUE)
message(sprintf('\nTop recommendations for you:\n'))

for (i in 1:10) {
    j = ix[i];
    message(sprintf('Predicting rating %.1f for movie %s\n', my_predictions[j],movieList[j]))
}

message(sprintf('\n\nOriginal ratings provided:\n'))

for (i in 1:length(my_ratings)) {
    if (my_ratings[i] > 0) 
    message(sprintf('Rated %d for %s\n', my_ratings[i],movieList[i]))
}

