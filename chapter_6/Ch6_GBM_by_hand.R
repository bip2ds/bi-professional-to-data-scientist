#######################
#Chapter 6, Listing XX to Listing XX
#######################

#### Set up ####
# Start by cleaning the environment
rm(list=ls())
dev.off()

# Set a random seed for reproducability
set.seed(42) 

# Libraries
library (rpart)
library(rpart.plot)
library(dplyr)
library(hydroGOF)

# Read in and format our data
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

#### Manual GBM ####

# Get a small sample
train_manual <- head(training, 50)

#### STEP 1: First prediction ####

# Compute first prediction
train_manual$f0_pred <- mean(train_manual$price_pw)

#### STEP 2: First residuals ####

# Compute first residuals
train_manual$f0_resid <- train_manual$price_pw - train_manual$f0_pred

#### STEP 3: Simple model ####

# Fit a simple decision tree
rpart_model <- rpart(f0_resid~.,data = train_manual[,-c(9,10)], method="anova" ,
                     control=rpart.control(maxdepth = 5),
                     model=T)

# Visualise the simple tree
rpart.plot(rpart_model,
           type=5,
           extra=101, 
           box.col="Green",
           shadow.col="gray",
           nn=T
)

#### STEP 4: Terminal Values ####

# Tree in tabular format
rpart_model$frame

# Which row in the tree each sample
as.vector(rpart_model$where)

# Allocate samples to nodes
train_manual$tree_id <-  row.names(rpart_model$frame)[rpart_model$where]

# Check the allcation numbers
table(train_manual$tree_id)

# Check one of the trees
tree_4 <- subset(train_manual, tree_id == "4")
mean(tree_4$f0_resid)

# Add terminal values
train_manual <- train_manual %>% 
  group_by(tree_id) %>% 
  mutate(f0_terminal = mean(f0_resid))

#### STEP 5: New predictions ####
alpha <- 0.1
train_manual$f1_pred <- train_manual$f0_pred + (alpha * train_manual$f0_terminal)

# Some new residuals, ready for next iteration
train_manual$f1_resid <- train_manual$price_pw - train_manual$f1_pred

# Some examples
train_manual[1:3,c(9,10,14,11,15)]

#### Check: RMSE ####

# After our first round
rmse(train_manual$f0_pred, train_manual$price_pw)

# After our second round
rmse(train_manual$f1_pred, train_manual$price_pw)
