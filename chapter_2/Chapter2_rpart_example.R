#######################
#Chapter 2, Listing XX to Listing XX
#######################

#Listing XXXX

#### SETUP ####

# Start by cleaning the environment
rm(list=ls())
dev.off()
# Set a random seed for reproducability
set.seed(42)

# Load necessary libraries
library(ISLR)
library (rpart)
library(rpart.plot)

# Load data
# Credit card default data
default_data <- Default
dim(default_data)
str(default_data)

# View data
str(default_data)

#### Split Data into Training and Test ####
# 1. set training dataset size to 75% of dataset
trainset_size <- floor(0.75 * nrow(default_data))
# 2. Randomly select rows to go into the training set
# This uses the sample() function
trainset_indices <- sample(seq_len(nrow(default_data)), size = trainset_size)
# 3. Use our sampled rows to segment data
trainset <- default_data[trainset_indices, ]
testset <- default_data[-trainset_indices, ]

# Rowcounts to check this process worked
nrow(trainset)
nrow(testset)
nrow(default_data)

#### Fit a simple decision tree model ####

# Run a simple decision tree. 
# This is a classification problem so set method="class"
simple_model <- rpart(default~.,data = trainset, method="class")

# Use prp() from rpart.plot to visualise the tree
prp(simple_model, digits=-1)

# Predict the target variable for the test set
rpart_predict <- predict(simple_model,testset[,-1],type="class")

# Cross-tabulate actual and predicted values (confusion matrix)
table(predicted=rpart_predict,actual=testset$default)
