gaussianKernel <- function (x1, x2, sigma=0.1) {
#RBFKERNEL returns a radial basis function kernel between x1 and x2
#   sim = gaussianKernel(x1, x2) returns a gaussian kernel between x1 and x2
#   and returns the value in sim

# Ensure that x1 and x2 are column vectors
x1 = as.vector(x1) 
x2 = as.vector(x2)

message("inside gaussianKernel")
print(x1)
print(x2)
print(sigma)

# You need to return the following variables correctly.
sim = 0

# ====================== YOUR CODE HERE ======================
# Instructions: Fill in this function to return the similarity between x1
#               and x2 computed using a Gaussian kernel with bandwidth
#               sigma
#
#

sim = exp(-1 * (sum((x1 - x2)^2) / (2 * (sigma^2))))




# =============================================================
}