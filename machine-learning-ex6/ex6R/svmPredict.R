svmPredict <- function (model, X, sigma=0.1) {
#SVMPREDICT returns a vector of predictions using a trained SVM model
#(svmTrain). 
#   pred = SVMPREDICT(model, X) returns a vector of predictions using a 
#   trained SVM model (svmTrain). X is a mxn matrix where there each 
#   example is a row. model is a svm model returned from svmTrain.
#   predictions pred is a m x 1 column of predictions of {0, 1} values.
#

source("linearKernel.R")    
source("gaussianKernel.R")    
    
    
# Check if we are getting a column vector, if so, then assume that we only
# need to do prediction for a single example
if (NCOL(X) == 1) {
    # Examples should be in rows
    X = t(X)
}

# Dataset 
m = NROW(X)
p = rep(0, m)
pred = rep(0, m)

#message(sprintf("svmPredict got m = %d", m))
#kfun <- deparse(substitute(model$kernelFunction))

kfun <- model$kernelFunction

if (strcmp(kfun, 'linearKernel')) {
# We can use the weights and bias directly if working with the 
# linear kernel
#    message("using linearKernel")
    p = X %*% model$w + model$b
}
else if (length(grep('gaussianKernel', kfun)) > 0) {
# Vectorized RBF Kernel
# This is equivalent to computing the kernel on every pair of examples
#    message("using gaussianKernel")
    X1 = matrix(rowSums(X^2), m, 1)
    X2 = matrix(rowSums(model$X^2), 1)
    K <- sweep(sweep(- 2 * (X %*% t(model$X)), 2, X2, FUN="+", check.margin=FALSE), 1, X1, FUN="+",check.margin=FALSE)
    kf <- match.fun(model$kernelFunction)
    g <- kf(1,0, sigma)
    K = g^ K

    K = sweep(K, 2, t(model$y), FUN = "*") 
    K = sweep(K, 2, t(model$alphas), FUN="*")
    p = matrix(rowSums(K), m, 1)
}
else {
#    message("using other non linear kernel")
    # Other Non-linear kernel
    for (i in 1:m) {
        prediction = 0;
        for (j in 1:nrow(model$X)) {
            prediction = prediction + model$alphas[j] * model$y[j] * match.fun(model$kernelFunction)(t(X[i,]), t(model$X[j,]))
        }
        p[i] = prediction + model$b
    }
}

# Convert predictions into 0 / 1
pred[p >= 0] =  1
pred[p <  0] =  0

pred
}

