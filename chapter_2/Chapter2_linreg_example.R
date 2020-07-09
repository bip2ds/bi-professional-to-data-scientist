#######################
#Chapter 2, Listing XX to Listing XX
#######################

#Listing XXXX

#### SETUP ####

# Start by cleaning the environment
rm(list=ls())
dev.off()
# Set a random seed for reproducability
set.seed(12)

# Load necessary libraries
library(ggplot2)

# Load data
# Marketing spend and sales figures from a business unit last quarter
sales_data <- read.csv(file="sales.csv")

# View data
str(sales_data)

#### Split Data into Training and Test ####

# 1. set training dataset size to 75% of dataset
trainset_size <- floor(0.75 * nrow(sales_data))
# 2. Randomly select rows to go into the training set
# This uses the sample() function
trainset_indices <- sample(seq_len(nrow(sales_data)), size = trainset_size)
# 3. Use our sampled rows to segment data
trainset <- sales_data[trainset_indices, ]
testset <- sales_data[-trainset_indices, ]

# Rowcounts to check this process worked
nrow(trainset)
nrow(testset)
nrow(sales_data)

#### Inspect Our Data ####

# Plot data
data_plot <- ggplot(trainset,aes(x=Spend,
                          y=Sales))+geom_point(colour="blue", size=3)
data_plot

#### Fit a simple linear regression model ####

# Create the model
lin_fit <- lm(formula = Sales ~ Spend,
              data = trainset)

# See model summary
summary(lin_fit)

# Add our line-of-best-fit to plot above
data_plot <- data_plot + geom_abline(
                     slope=as.numeric(lin_fit$coefficients[2]),
                     intercept=as.numeric(lin_fit$coefficients[1]))
data_plot

# Plot test points
data_plot <- data_plot + geom_point(
                        data=testset,aes(x=Spend,y=Sales),
                        size=4,colour="red")
data_plot

#### Fit a more curved model ####

# Start with degree 3 
data_plot <- data_plot + geom_smooth(method = lm, 
                                     formula = y ~ poly(x,3), 
                                     se = FALSE, color="green")
data_plot

# Try degree 5
data_plot <- data_plot + geom_smooth(method = lm, 
                                     formula = y ~ poly(x,5), 
                                     se = FALSE, color="brown")
data_plot

# Try degree 7
data_plot <- data_plot + geom_smooth(method = lm, 
                                     formula = y ~ poly(x,7), 
                                     se = FALSE, color="pink")
data_plot

# Let's zoom-in by fixing the y-axis
data_plot <- data_plot + coord_cartesian(ylim = c(140,210))
data_plot 

#### How did our Basic model do? ####

# Predict on our test points
testset$predictions <- predict.lm(lin_fit, 
                                  newdata = testset, 
                                  type="response")

# Take difference predicted to actual
testset$difference <- testset$Sales - testset$predictions
testset$difference

# Take absolute value and average
average_difference = sum(abs(testset$difference)) / length(testset$difference)
average_difference

#### How did our 3rd Degree model do? ####

# Create the model
lin_fit3 <- lm(formula = Sales ~ poly(Spend,3),
              data = trainset)

# Predict on our test points
testset$predictions3 <- predict.lm(lin_fit3, 
                                  newdata = testset, 
                                  type="response")

# Take difference predicted to actual
testset$difference3 <- testset$Sales - testset$predictions3
testset$difference3

# Take absolute value and average
average_difference3 = sum(abs(testset$difference3 )) / length(testset$difference3 )
average_difference3