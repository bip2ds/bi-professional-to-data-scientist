#######################
#Chapter 6, Listing XX to Listing XX
#######################

#### Set up ####
# Start by cleaning the environment
rm(list=ls())
def.off()

# Set a seed
set.seed(42)

# Libraries
library(randomForest)
library(ggplot2)

# Load data & run model
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
# Build a Random Forest
wines_rf <- randomForest(quality_binary ~.
                         ,data = trainset, 
                         importance=TRUE
                         , xtest=testset[,-ncol(wine)]
                         ,ntree=500,
                         keep.forest=TRUE)

#### Partial Dependency Plots ####

# For alcohol
pdp_alc_pos <- partialPlot(x=wines_rf, pred.data=trainset, x.var = 'alcohol', which.class = 1
                          )
pdp_alc_neg <- partialPlot(x=wines_rf, pred.data=trainset, x.var = 'alcohol', which.class = 0)

# Helper function for logits to probability conversion
convert_logits <- function(logit){
  odds_ratio <- exp(logit)
  probability <- odds_ratio / (1 + odds_ratio)
  probability
}

# Turn logits into probabilities
x_vals_alc_pos <- pdp_alc_pos$x
y_vals_alc_pos <- convert_logits(pdp_alc_pos$y)
x_vals_alc_neg <- pdp_alc_neg$x
y_vals_alc_neg <- convert_logits(pdp_alc_neg$y)

# Create our own plot (Positive)
p_pos <- ggplot() + geom_line(aes(x_vals_alc_pos, y_vals_alc_pos), linetype="solid", lwd=0.7, color='blue')+ theme(plot.margin=unit(c(2,2,2,2),"cm")) + ggtitle("PDP of Alcohol on Positive Class") + theme(plot.title = element_text(hjust = 0.5)) + xlab("Alcohol") + ylab("Probability of Positive Class")
p_pos

# Create our own plot (Negative)
p_neg <- ggplot() + geom_line(aes(x_vals_alc_neg, y_vals_alc_neg), linetype="solid", lwd=0.7, color='red')+ theme(plot.margin=unit(c(2,2,2,2),"cm")) + ggtitle("PDP of Alcohol on Negative Class") + theme(plot.title = element_text(hjust = 0.5)) + xlab("Alcohol") + ylab("Probability of Negative Class")
p_neg