

# Linear model selection

library(tidyverse)
library(leaps)
library(ISLR)

Hitters <- na.omit(Hitters) %>% as_tibble()

# Best subset method

best_subset <- regsubsets(Salary~., Hitters, nvmax = 19)
summary(best_subset)
# slow process when no. of predictors are very large.
# statistical problems: larger search space, look good model on training data
# high variance, overfitting

forward <- regsubsets(Salary~., Hitters, nvmax = 19, method = "forward")
summary(forward)
