mapFeature <- function (X1, X2) {
# MAPFEATURE Feature mapping function to polynomial features
#
#  MAPFEATURE(X1, X2) maps the two input features
#  to quadratic features used in the regularization exercise.
#
#   Returns a new feature array with more features, comprising of 
#   X1, X2, X1.^2, X2.^2, X1*X2, X1*X2.^2, etc..
#
#   Inputs X1, X2 must be the same size
#

degree = 6 
out = matrix(1, nrow=NROW(X1), ncol=1)   #for X0


for (i in 1:degree) {
    for (j in 0:i) {
        out = cbind(out, as.vector((X1 ^(i-j)) *(X2 ^j)))
#        out(:, end+1) = (X1.^(i-j)).*(X2.^j);
    }
}

return(out)

}