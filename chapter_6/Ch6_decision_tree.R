#######################
#Chapter 6, Listing XX to Listing XX
#######################

#### Set up ####
# Start by cleaning the environment
rm(list=ls())

# Set a random seed for reproducability
set.seed(42) 

# Load packages
library (rpart)
library(rpart.plot)

# Load the cardiac dataset
hearts <- readRDS("hearts.rds")

# Explore the dataset
dim(hearts)
summary(hearts)

# Check the data structure
str(hearts)

# Split data

# Split dataset
trainset_size <- floor(0.75 * nrow(hearts))
trainset_indices <- sample(seq_len(nrow(hearts)), size = trainset_size)

# Assign observations to training and testing sets
trainset <- hearts[trainset_indices, ]
testset <- hearts[-trainset_indices, ]

# Rowcounts to check
nrow(trainset)
nrow(testset)
nrow(hearts)

#### Build a tree ####

# A simple decision tree. Classification problem sets method="class"
rpart_model <- rpart(event~.,data = trainset, method="class")

# Summary
summary(rpart_model)

# Plot tree (Version 1 - basic plot)
plot(rpart_model)
text(rpart_model)

# Plot tree (Version 2 - prp from rpart.plot)
prp(rpart_model)


# Plot tree (Version 3 - extended visualisations)
# Customise as per https://www.rdocumentation.org/packages/rpart.plot/versions/3.0.6/topics/rpart.plot
rpart.plot(rpart_model,
           type=2,
           extra=101, 
           box.palette="GnBu",
           shadow.col="gray"
)

# Make a more complex model
rpart_model_complex <- rpart(event~.,data = trainset, 
                     method="class", 
                     control=rpart.control(minsplit = 5, cp = 0.001))

# Score our different models 
score_model <- function(model){
  # Predict on test data
  class_predictions <- predict(model, 
                               newdata = testset[, -ncol(testset)]
                               , type = "class")
  
  # Accuracy
  accuracy <- mean(class_predictions==testset$event)
  #confusion matrix
  cfm <- table(pred=class_predictions,true=testset$event) 
  #Precision = TP/(TP+FP)
  precision <- cfm[1,1]/(cfm[1,1]+cfm[1,2]) 
  
  #Recall = TP/(TP+FN)
  recall <- cfm[1,1]/(cfm[1,1]+cfm[2,1]) 
  #F1
  f1 <- 2*(precision*recall/(precision+recall)) 
  # Print model results
  cat(sprintf(
    "
           Model Results
    Accuracy: %s
    Precision: %s
    recall: %s
    F1: %s
        
       Confusion Matrix
              True 
                0      1
    Pred    0   %s    %s
            1   %s     %s
    ",round(accuracy,2), round(precision,2), round(recall,2), round(f1, 2)
    , cfm[1,1], cfm[1,2], cfm[2,1], cfm[2,2]))
}

# Score the models
score_model(rpart_model)
score_model(rpart_model_complex)

# Make a very complex model
rpart_model_very_complex <- rpart(event~.,data = trainset, 
                             method="class", 
                             control=rpart.control(minsplit = 5, cp = 0.001))


# Score the very complex model
score_model(rpart_model_very_complex)

### PAUSE - Try different Seeds above ####

# Set a different seed and run the model again.

#### Different partitions ####

multiple_runs_rpart <- function(df,target,train_fraction,nruns){
  
  # Initialize accuracy vector
  accuracies <- rep(NA,nruns)
  
  # Set seed for reproducability inside the for loop
  set.seed(42)
  
  # Loop through nrun times
  for (i in 1:nruns){
    # Partition data
    trainset_size <- floor(train_fraction * nrow(df))
    trainset_indices <- sample(seq_len(nrow(df)), size = trainset_size)
    trainset <- df[trainset_indices, ]
    testset <- df[-trainset_indices, ]
    # Build model
    # Paste builds formula string and as.formula interprets it as an R formula
    rpart_model <- rpart(
                  as.formula(paste(target,"~.")),
                  data = trainset, 
                  method="class")
    # Predict on test data
    rpart_predict <- predict(rpart_model, 
                             newdata = testset[, -ncol(testset)]
                             , type = "class")
    # Accuracy
    accuracies[i] <- mean(rpart_predict==testset[["event"]])
  }
  return(accuracies)
}

# Calculate average accuracy and std dev over 50 random partitions
accuracy_results <- multiple_runs_rpart(hearts,"event",0.75,50)
accuracy_results
cat(min(accuracy_results), max(accuracy_results), "\n")
mean(accuracy_results)
sd(accuracy_results)


