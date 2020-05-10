#######################
#Chapter 6, Listing XX to Listing XX
#######################

#### Set up ####
# Start by cleaning the environment
rm(list=ls())

# Necessary libraries
library(randomForest)

# Set a random seed for reproducability
set.seed(42) 

# Read in dataset
wine <- read.csv("wines.csv")

# Drop the ID column
wine$ID <- NULL

# Let's make this a binary classification
wine$quality_binary <- 1
wine[wine$quality < 7, "quality_binary"] <- 0
wine$quality_binary <- as.factor(wine$quality_binary)

# Need to drop the quality predictor
wine$quality <- NULL

# Split dataset
trainset_size <- floor(0.75 * nrow(wine))
trainset_indices <- sample(seq_len(nrow(wine)), size = trainset_size)

# Assign observations to training and testing sets
trainset <- wine[trainset_indices, ]
testset <- wine[-trainset_indices, ]

# Rowcounts to check
nrow(trainset)
nrow(testset)
nrow(wine)

#### Build a Random Forest ####

# Build random forest model
wines_rf <- randomForest(quality_binary ~.
                         ,data = trainset, 
                         importance=TRUE
                         , xtest=testset[,-ncol(wine)]
                         ,ntree=50)

# Model summary
# Not super useful for model analysis 
summary(wines_rf) 

# Objects returned from the model 
names(wines_rf)
names(wines_rf$test)

# Predictions for test set
testset$predicted_class <- wines_rf$test$predicted

# Accuracy for test set
mean(testset$predicted_class==testset$quality_binary)

# Confusion matrix
cfm <- table(predicted=testset$predicted_class,true=testset$quality_binary)
cfm

# Precision = TP/(TP+FP)
precision <- cfm[1,1]/(cfm[1,1]+cfm[1,2])
precision

# Recall = TP/(TP+FN)
recall <- cfm[1,1]/(cfm[1,1]+cfm[2,1])
recall

# F1
f1 <- 2*(precision*recall/(precision+recall))
f1

# Quantitative measure of variable importance
importance(wines_rf)

# Sorted plot of importance
varImpPlot(wines_rf)

