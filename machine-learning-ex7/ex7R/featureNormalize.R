featureNormalize <- function (X) {
#FEATURENORMALIZE Normalizes the features in X 
#   FEATURENORMALIZE(X) returns a normalized version of X where
#   the mean value of each feature is 0 and the standard deviation
#   is 1. This is often a good preprocessing step to do when
#   working with learning algorithms.
#   it return [X_norm, mu, sigma]
#
mu = colMeans(X)
X_norm = sweep(X, 2, mu)

sigma = apply(X_norm, 2, sd)
X_norm = sweep(X_norm, 2, sigma, "/")


# ============================================================

normX <- list(X_norm=X_norm, mu=mu, sigma=sigma)

}    
