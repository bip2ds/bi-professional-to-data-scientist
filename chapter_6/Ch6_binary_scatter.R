#######################
#Chapter 6, Listing XX to Listing XX
#######################

#### Set up
# Start by cleaning the environment
rm(list=ls())

# Set a random seed for reproducability
set.seed(42) 

# We will use heart disease data
data <- read.csv("hearts.csv")

# ggplot for simple viz
library(ggplot2)


#### Plot

# Basic scatter plot
ggplot(data, 
       aes(x=thalach, y=hd)) + geom_point(shape=18, color="blue") +ggtitle("Heart Rate vs Healthy Heart") + theme(plot.title = element_text(hjust = 0.5)) + theme(plot.margin=unit(c(2,2,2,2),"cm"))

# Add a basic regression line 
ggplot(data, 
       aes(x=thalach, y=hd)) + geom_point(shape=18, color="blue") + geom_smooth( method=lm, se=FALSE, linetype="solid", color="red") +ggtitle("Heart Rate vs Healthy Heart Including Regression") + theme(plot.title = element_text(hjust = 0.5)) + theme(plot.margin=unit(c(2,2,2,2),"cm"))

# Try with a sigmoid curve 
ggplot(data, 
       aes(x=thalach, y=hd)) + geom_point(shape=18, color="blue") + geom_smooth( method="glm", se=FALSE, linetype="solid", color="red",method.args = list(family = "binomial") ) +ggtitle("Heart Rate vs Healthy Heart Including Sigmoid") + theme(plot.title = element_text(hjust = 0.5)) + theme(plot.margin=unit(c(2,2,2,2),"cm"))
