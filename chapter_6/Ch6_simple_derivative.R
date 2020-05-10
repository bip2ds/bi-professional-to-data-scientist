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
library(ggplot2)

#### Simple derivative example ####

# Set up our function
x_vals <- seq(-10,10,0.5)

loss_func <- function(x){
  y <- (0.5) * x^2
  y
}

y_vals <- loss_func(x_vals)

# Plot our loss function
p <- ggplot() + geom_line(aes(x_vals, y_vals), linetype="solid", lwd=1.5, color='blue')+ theme(plot.margin=unit(c(2,2,2,2),"cm"))
p

# Our first point
p <- p + geom_point(aes(5, 12.5), color="red", size=5)
p

# Naiive Gradient
naive_gradient <- function(x,dx){
  return ((
    (loss_func(x+dx)- loss_func(x) )/ dx
  ))
}
naive_gradient(5,0.0001)

# A second point
p <- p + geom_point(aes(-7, 24.5), color="green", size=5)
p
