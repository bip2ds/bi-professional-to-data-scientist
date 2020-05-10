#######################
#Chapter 6, Listing XX to Listing XX
#######################

#### Set up
# Start by cleaning the environment
rm(list=ls())

# Set a random seed for reproducability
set.seed(42) 

# ggplot for viz
library(ggplot2)

#### Create a function
sigmoid = function(x) {
  1 / (1 + exp(-x))
}

#### Create test values
x_vals <- x <- seq(-10, 10, 0.1)

#### Use our function
y_vals <- sigmoid(x_vals)

#### Zip up our data
data <- data.frame(x_vals,y_vals)

#### Plot the result
ggplot(data, 
       aes(x=x_vals, y=y_vals)) + geom_point(shape=18, color="blue") +ggtitle("Our Sigmoid Curve") + theme(plot.title = element_text(hjust = 0.5)) + theme(plot.margin=unit(c(2,2,2,2),"cm"))

