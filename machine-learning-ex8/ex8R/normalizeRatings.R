#
#
#
normalizeRatings <- function(Y, R) {
#NORMALIZERATINGS Preprocess data by subtracting mean rating for every 
#movie (every row)
#   [Ynorm, Ymean] = NORMALIZERATINGS(Y, R) normalized Y so that each movie
#   has a rating of 0 on average, and returns the mean rating in Ymean.
#

m = nrow(Y)
n = ncol(Y)

Ymean = matrix(0, m, 1)
Ynorm = matrix(0, m, n)
for (i in 1:m) {
    idx = which(R[i, ] == 1)
    Ymean[i] = mean(Y[i, idx])
    Ynorm[i, idx] = Y[i, idx] - Ymean[i]
}

MandN = list(Ynorm=Ynorm, Ymean=Ymean)
}