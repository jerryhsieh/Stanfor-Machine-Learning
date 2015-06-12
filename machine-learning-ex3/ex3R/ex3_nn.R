## Machine Learning Online Class - Exercise 3 | Part 2: Neural Networks

#  Instructions
#  ------------
# 
#  This file contains code that helps you get started on the
#  linear exercise. You will need to complete the following functions 
#  in this exericse:
#
#     lrCostFunction.m (logistic regression cost function)
#     oneVsAll.m
#     predictOneVsAll.m
#     predict.m
#
#  For this exercise, you will not need to change any code in this file,
#  or any other files other than those mentioned above.
#

## Initialization
#clear ; close all; clc
rm(list=ls())

## Setup the parameters you will use for this exercise
input_layer_size  = 400   # 20x20 Input Images of Digits
hidden_layer_size = 25    # 25 hidden units
num_labels = 10           # 10 labels, from 1 to 10   
# (note that we have mapped "0" to label 10)

## =========== Part 1: Loading and Visualizing Data =============
#  We start the exercise by first loading and visualizing the dataset. 
#  You will be working with a dataset that contains handwritten digits.
#
library(R.matlab)
source("displayData.R")

# Load Training Data
message(sprintf('Loading and Visualizing Data ...\n'))

data <- readMat('../ex3/ex3data1.mat')
X <- data$X
y <- data$y

m = NROW(X)

# Randomly select 100 data points to display
#sel = randperm(size(X, 1));
sel = sample(m)
sel = sel[1:100]

displayData(X[sel, ])

readline(prompt='Program paused. Press enter to continue.\n')

## ================ Part 2: Loading Pameters ================
# In this part of the exercise, we load some pre-initialized 
# neural network parameters.

message(sprintf('\nLoading Saved Neural Network Parameters ...\n'))

# Load the weights into variables Theta1 and Theta2
data <- readMat('../ex3/ex3weights.mat')

Theta1 <- data$Theta1
Theta2 <- data$Theta2


## ================= Part 3: Implement Predict =================
#  After training the neural network, we would like to use it to predict
#  the labels. You will now implement the "predict" function to use the
#  neural network to predict the labels of the training set. This lets
#  you compute the training set accuracy.
source("predict.R")


pred = predict(Theta1, Theta2, X)

message(sprintf('\nTraining Set Accuracy: %f\n', mean((pred == y) * 100)))

readline(prompt='Program paused. Press enter to continue.\n')

#  To give you an idea of the network's output, you can also run
#  through the examples one at the a time to see what it is predicting.

#  Randomly permute examples
#rp = randperm(m);
source("predict.R")


rp = sample(m)

for (i in 1:m) {
    # Display 
    message(sprintf('\nDisplaying Example Image\n'))
    displayData(matrix(X[rp[i], ], nrow=1))

    pred = predict(Theta1, Theta2, matrix(X[rp[i],], nrow=1))
    message(sprintf('\nNeural Network Prediction: %d (digit %d)\n', pred, mod(pred, 10)))
    
    # Pause
    readline(prompt='Program paused. Press enter to continue.\n')
}

