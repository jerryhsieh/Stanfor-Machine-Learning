predict <- function (Theta1, Theta2, X) {
#PREDICT Predict the label of an input given a trained neural network
#   p = PREDICT(Theta1, Theta2, X) outputs the predicted label of X given the
#   trained weights of a neural network (Theta1, Theta2)

# Useful values
m = NROW(X)
num_labels = NROW(Theta2)

# You need to return the following variables correctly 
#p = zeros(size(X, 1), 1);

# ====================== YOUR CODE HERE ======================
# Instructions: Complete the following code to make predictions using
#               your learned neural network. You should set p to a 
#               vector containing labels between 1 to num_labels.
#
# Hint: The max function might come in useful. In particular, the max
#       function can also return the index of the max element, for more
#       information see 'help max'. If your examples are in rows, then, you
#       can use max(A, [], 2) to obtain the max for each row.
#

source("sigmoid.R")

#X = [ones(m, 1) X];

X <- cbind(rep(1, m), X)

A2 = sigmoid(X %*% t(Theta1))

#A2 = [ones(m, 1) A2];

A2 = cbind(rep(1, m), A2)

A3 = sigmoid(A2 %*% t(Theta2))

#[v p] = max(A3, [], 2);

p <- max.col(A3)

# =========================================================================
    
}
