dataset3Params <- function (X, y, Xval, yval) {
#EX6PARAMS returns your choice of C and sigma for Part 3 of the exercise
#where you select the optimal (C, sigma) learning parameters to use for SVM
#with RBF kernel
#   [C, sigma] = EX6PARAMS(X, y, Xval, yval) returns your choice of C and 
#   sigma. You should complete this function to return the optimal C and 
#   sigma based on a cross-validation set.
#

# You need to return the following variables correctly.
C = 1
sigma = 0.3

# ====================== YOUR CODE HERE ======================
# Instructions: Fill in this function to return the optimal C and sigma
#               learning parameters found using the cross validation set.
#               You can use svmPredict to predict the labels on the cross
#               validation set. For example, 
#                   predictions = svmPredict(model, Xval);
#               will return the predictions on the cross validation set.
#
#  Note: You can compute the prediction error using 
#        mean(double(predictions ~= yval))
#

Ct = c(0.01,0.03,0.1,0.3,1,3,10,30)
sigt = Ct

perror = matrix(0, length(Ct), length(sigt))


for (i in 1:length(Ct)) {
    for (j in 1:length(sigt)) {
    
        message(sprintf('in loop %d and %d\n', i, j))

        model = svmTrain(X, y, Ct[i], gaussianKernel, sigt[j]) 
        predictions = svmPredict(model, Xval, sigt[j])
        perror[i, j] = mean(predictions != yval)

    }
}

#[val, idx] = min (perror (:));
#[i, j] = ind2sub (size (perror), idx); 

iandj <- which(perror == min(perror), arr.ind = TRUE)  
i <- iandj[1,1]
j <- iandj[1,2]

C = Ct[i]
sigma = sigt[j]

CandS = list(C, sigma)
# =========================================================================
    
}
