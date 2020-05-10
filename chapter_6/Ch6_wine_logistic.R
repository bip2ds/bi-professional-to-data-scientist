#######################
#Chapter 6, Listing XX to Listing XX
#######################

#### Set up ####
# Start by cleaning the environment
rm(list=ls())

# Set a random seed for reproducability
set.seed(42) 

# We will use wine data
wine <- read.csv("wines.csv")

# Check the data structure
str(wine)

# Drop the ID column
wine$ID <- NULL

# Let's make this a binary classification
wine$quality_binary <- 1
wine[wine$quality < 7, "quality_binary"] <- 0
wine$quality_binary <- as.factor(wine$quality_binary)

# Need to drop the quality predictor
wine$quality <- NULL

# Check 
str(wine)

# What proportion do we have?
prop.table(table(wine$quality_binary))

# See scaling
summary(wine)

# Scale the dataset
scale_100 <- function(data) {
  data <- (data-min(data))/(max(data) - min(data)) 
  data <- data * 100
}

library(dplyr)
wine <- wine %>%
  mutate_if(is.numeric, scale_100)
summary(wine)

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

#### A Simple Model ####

# Let's start by using all variables
wine_logistic = glm(
             formula = quality_binary ~ .,
             data = trainset,
             family = "binomial")

# Analyse the model output
summary(wine_logistic)

# What else is available in a model output?
names(wine_logistic)

# Turn coefficient logits into probabilities
exp(coef(wine_logistic)) * 100 -100

#### Evaluation ####

# Create probabilities and predictions

# Probabilities firstly
testset$probability = predict(wine_logistic, 
                              newdata = testset[, -ncol(testset)]
                              , type = "response")

# Set a probability threshold of 0.5 to create class prediction
testset$class_prediction = 0
testset[testset$probability >= 0.5, "class_prediction"] = 1 

# Create an accuracy score
accuracy <- mean(testset$class_prediction == testset$quality_binary)
accuracy

#### Further Evaluation ####

# Create a confusion matrix (along with other measures) using the table function
cfm <- table(predicted=testset$class_prediction,true=testset$quality_binary)
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

#### Cut-off Curves ####

# Prediction Objects
library(ROCR)

# We need our target to be a factor with levels
testset$quality_binary <- as.factor(testset$quality_binary)
levels(testset$quality_binary) <- c("Bad", "Good")

# Create a prediction object on the testing data
test_pred <- prediction(testset$probability, testset$quality_binary)

# Get the sensitivity and specificity object
sens <- performance(test_pred, "sens")
spec <- performance(test_pred, "spec")

# Plot our curves
# Firstly set up the graph and sensitivity
plot(
  sens, 
  main = "Sensitivity Specificity Chart", type = "l", col = "red", lwd = 2, 
  xlim = c(0,1), ylim = c(0,1), 
  ylab = "Values")
  axis(side = 1, at = seq(0, 1, 0.1))
  axis(side = 2, at = seq(0, 1, 0.1))
  
# Add a legend & some grid lines
legend("topright", legend = c("Sensitivity","Specificity"), col = c("red", "blue"), lty = 1, lwd = 2, inset=c(0.01,0.5))

abline(h = seq(0, 1, 0.1), v = seq(0, 1, 0.1), col="gray", lty=3)
  
# Add specificity
plot(
  spec, add = T, col = "blue", lwd = 2, 
  xlim = c(0,1), ylim = c(0,1)
)

# Find the optimal cutoff

# Create a new performance object for ease
test_sens_spec = performance(test_pred, "sens","spec")

# A dataframe of key values
threshold_df = data.frame(cut = test_sens_spec@alpha.values[[1]], 
                          sens = test_sens_spec@x.values[[1]],
                          spec = test_sens_spec@y.values[[1]])
# See some values
print(head(threshold_df,5))
print(tail(threshold_df, 5))

# Find row that is maximum of summing sensitivity and specificity
which.max(threshold_df$sens + threshold_df$spec)

# Roll the above into a simple subset to get the relevant cutoff value
threshold = threshold_df[which.max(threshold_df$sens + threshold_df$spec), "cut"]

cat("\nProbability threshold is", threshold)

# How does this affect accuracy?
accuracy # Reminder of old accuracy
testset$class_prediction_opt = "Bad"
testset[testset$probability >= threshold, "class_prediction_opt"] = "Good"

# Create a new accuracy score
accuracy_opt <- mean(testset$class_prediction_opt == testset$quality_binary)
accuracy_opt

# Create a new confusion matrix
cfm_opt <- table(predicted=testset$class_prediction_opt,true=testset$quality_binary)
cfm_opt