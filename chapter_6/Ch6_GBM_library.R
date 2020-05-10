#######################
#Chapter 6, Listing XX to Listing XX
#######################

#### Set up ####
# Start by cleaning the environment
rm(list=ls())
dev.off()
library(gbm)
library(hydroGOF)

# Set a random seed
set.seed(42) 

# Read in data
rent_data <- readRDS("rent_data.rds")

# Create training and test sets. This process should be familiar by now
trainset_size <- floor(0.80 * nrow(rent_data))
trainset_indices <- sample(seq_len(nrow(rent_data)), size = trainset_size)
training <- rent_data[trainset_indices, ]
testset <- rent_data[-trainset_indices, ]

# Checks
nrow(training)
nrow(testset)
nrow(rent_data)

#### Basic Model ####

# Fit initial model
gbm_rs = gbm(training$price_pw~.,
              data=training[, -ncol(training)],
              distribution='gaussian', # Regression problem
              interaction.depth= 5, # Maximum nodes per tree
              n.minobsinnode = 15, # Minimum number of observations in the terminal nodes
              shrinkage=0.01, # Learning rate 
             n.trees=2000, # Number of iterations
             cv.folds = 5, # Number of cross-validation folds
              verbose = TRUE # Print the preliminary output
)

# Score basic model
testset$pred_basic = predict(gbm_rs, testset[, -ncol(testset)], n.trees = gbm_rs$n.trees , type = "response")
rmse(testset$pred_basic, testset$price_pw)

#### Optimal Iterations Model #### 

# Estimate the optimal number of iterations (when will the model stop improving)
best_iter = gbm.perf(gbm_rs, method = "cv")
print(best_iter)

#### Score Optimal Iterations Model ####

# Score with optimal number of trees used
testset$pred_opt = predict(gbm_rs, testset, n.trees = best_iter, type = "response")
rmse(testset$pred_opt, testset$price_pw)

#### Variable Importance ####

# Gives the variable importance in a graph
summary(gbm_rs,n.trees=best_iter, ylab = "Variable", main = "Variable Relative Importance")

# OR just as a table
summary(gbm_rs)

