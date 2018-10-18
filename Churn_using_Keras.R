
# install required libraries
rm(list = ls())

pkgs <- c("keras", "lime", "tidyquant", "rsample", "recipes", "yardstick", "corrr")
install.packages(pkgs)

# Load libraries
library(keras)
library(lime)
library(tidyquant)
library(rsample)
library(recipes)
library(yardstick)
library(corrr)

# Data import

churn <- read_csv("E:\\Study\\R Projects\\Common files\\WA_Fn-UseC_-Telco-Customer-Churn.csv")
glimpse(churn)


# Remove unnecessary data
names(churn)
churn_data_tbl <- churn %>%
  select(-customerID) %>%
  drop_na() %>%
  select(Churn, everything())

glimpse(churn_data_tbl)


# Split test/training sets
set.seed(100)
train_test_split <- initial_split(churn_data_tbl, prop = 0.8)
train_test_split


# Retrieve train and test sets
train_tbl <- training(train_test_split)
test_tbl  <- testing(train_test_split)

## Artificial Neural Networks are best when the data is one-hot encoded, scaled and centered

# DISCRETIZE THE "TENURE" FEATURE
library(ggplot2)
train_tbl %>%
  group_by(tenure) %>%
  tally() %>%
  ggplot(aes(x=tenure, y=n)) +
  geom_bar(stat = "identity") +
  labs(title = "Tenure count without binning", y = "count")

# Total charge histogram
train_tbl %>%
  group_by(TotalCharges) %>%
  tally() %>%
  ggplot(aes(x=TotalCharges)) +
  geom_histogram(binwidth = 100) +
  labs(title = "TotalCharge", y = "count")

# Log transformed 'Total charge'
train_tbl %>%
  group_by(TotalCharges) %>%
  tally() %>%
  ggplot(aes(x=TotalCharges)) +
  geom_histogram(bins = 100) +
  scale_x_log10() +
  labs(title = "TotalCharge", y = "count")

### A quick test is to see if the log transformation
## increases the magnitude of the correlation between "TotalCharges" and "Churn" (corr)

# Determine if log transformation improves correlation 
# between TotalCharges and Churn
train_tbl %>%
  select(Churn, TotalCharges) %>%
  mutate(
    Churn = Churn %>% as.factor() %>% as.numeric(),
    LogTotalCharges = log(TotalCharges)
  ) %>%
  correlate() %>%                   # tidy correlations on numeric data
  focus(Churn) %>%                  #similar to select(). Takes and focues on only rows/columns of importance
  fashion()                         # formatting to read

## Log transformation has more correlation - it should improve the accuracy of ANN


# One hot encoding required

names(train_tbl)
train_tbl %>%
  group_by(Contract) %>%
  tally() %>%
  ggplot(aes(x=as.factor(Contract), y=n)) +
  geom_bar(stat = "identity") +
  labs(title = "Contract", y = "count")

train_tbl %>%
  group_by(InternetService) %>%
  tally() %>%
  ggplot(aes(x=as.factor(InternetService), y=n)) +
  geom_bar(stat = "identity") +
  labs(title = "InternetService", y = "count")

train_tbl %>%
  group_by(MultipleLines) %>%
  tally() %>%
  ggplot(aes(x=as.factor(MultipleLines), y=n)) +
  geom_bar(stat = "identity") +
  labs(title = "MultipleLines", y = "count")

train_tbl %>%
  group_by(PaymentMethod) %>%
  tally() %>%
  ggplot(aes(x=as.factor(PaymentMethod), y=n)) +
  geom_bar(stat = "identity") +
  labs(title = "PaymentMethod", y = "count")

# ANN's(use gradient descent) perform faster and (often) with higher accuracy when features are scaled 
# and/or normalized.

# Create recipe
rec_obj <- recipe(Churn ~ ., data = train_tbl) %>%   # to implement preprocessing steps
  step_discretize(tenure, options = list(cuts = 6)) %>%    # cut and cohort creation
  step_log(TotalCharges) %>%                             # log transform
  step_dummy(all_nominal(), -all_outcomes()) %>%         # one hot encoding
  step_center(all_predictors(), -all_outcomes()) %>%
  step_scale(all_predictors(), -all_outcomes()) %>%
  prep(data = train_tbl)      # estimate the required parameters from a training set that can later be applied to other data sets

rec_obj

# Predictors
x_train_tbl <- bake(rec_obj, newdata = train_tbl)    # "recipe" to any data set with the bake() function
x_test_tbl  <- bake(rec_obj, newdata = test_tbl)

glimpse(x_train_tbl)


# Response variables for training and testing sets
y_train_vec <- ifelse(pull(train_tbl, Churn) == "Yes", 1, 0)  # train_tbl$Churn
y_test_vec  <- ifelse(pull(test_tbl, Churn) == "Yes", 1, 0)

# delete 'churn' from tables

x_train_tbl$Churn <- NULL
x_test_tbl$Churn <- NULL

####################### Model Building (Multi-Layer Perceptron) ######################

# 3 layer MLP
# initialize a sequential model
# input layer is the data
# apply layers to sequential model (hidden, dropout, output)
# compile model

# Building Keras MLP-flavored ANN model
library(keras)
model_keras <- keras_model_sequential()    # beginning of model; linear stack of layers

model_keras %>% 
  # First hidden layer
  layer_dense(                                       # hidden layer created
    units              = 16,                         # no of nodes
    kernel_initializer = "uniform", 
    activation         = "relu", 
    input_shape        = ncol(x_train_tbl)) %>%      # no of columns in training set
  layer_dropout(rate = 0.1) %>%                      # to control overfitting
  # Second hidden layer
  layer_dense(
    units              = 16, 
    kernel_initializer = "uniform", 
    activation         = "relu") %>% 
  layer_dropout(rate = 0.1) %>%
  # Output layer
  layer_dense(                                       # shape of o/p and method of assimilating the learned info
    units              = 1,                          # binary =1; multi-class: units=no. of classes
    kernel_initializer = "uniform", 
    activation         = "sigmoid") %>%              # common for binary classification
  # Compile ANN
  compile(                                           # compile: to incorporate backpropagation to DNN
    optimizer = 'adam',                              # optimization algorithm
    loss      = 'binary_crossentropy',               # optimizer is included in tuning process
    metrics   = c('accuracy'))                       # loss: objective function
model_keras                                          # reg:MSE and clss:cross_entropy

# Fit the keras model to the training data
fit_keras <- fit(
  object           = model_keras,                    # created model
  x                = as.matrix(x_train_tbl),         # matric format  
  y                = y_train_vec,
  batch_size       = 50,                             # no. of samples per gradient update within each epoch
  epochs           = 35,                             # no. of training cycles
  validation_split = 0.30)                           # 30% of data for model validation, prevents overfitting

plot(fit_keras)
## Only use enough epochs to get a high validation accuracy. 
## Once validation accuracy curve begins to flatten or decrease, it's time to stop training.

# Predicted Class
yhat_keras_class_vec <- predict_classes(object = model_keras, x = as.matrix(x_test_tbl)) %>%
  as.vector()

# Predicted Class Probability
yhat_keras_prob_vec  <- predict_proba(object = model_keras, x = as.matrix(x_test_tbl)) %>%
  as.vector()

### Performance inspection 

estimates_keras_tbl <- tibble(
  truth      = as.factor(y_test_vec) %>% fct_recode(yes = "1", no = "0"),
  estimate   = as.factor(yhat_keras_class_vec) %>% fct_recode(yes = "1", no = "0"),
  class_prob = yhat_keras_prob_vec
)

estimates_keras_tbl
options(yardstick.event_first = FALSE)             # the default is to classify 0 as the positive class instead of 1


# Confusion Table
estimates_keras_tbl %>% conf_mat(truth, estimate)
# Accuracy
estimates_keras_tbl %>% metrics(truth, estimate)
# AUC
estimates_keras_tbl %>% roc_auc(truth, class_prob)
# Precision
tibble(
  precision = estimates_keras_tbl %>% precision(truth, estimate),
  recall    = estimates_keras_tbl %>% recall(truth, estimate)
)
# F1-Statistic
estimates_keras_tbl %>% f_meas(truth, estimate, beta = 1)


####################### LIME ###########################################

class(model_keras)
# Setup lime::model_type() function for keras
model_type.keras.models.Sequential <- function(x, ...) {
  return("classification")
}

# Setup lime::predict_model() function for keras
predict_model.keras.models.Sequential <- function(x, newdata, type, ...) {
  pred <- predict_proba(object = x, x = as.matrix(newdata))
  return(data.frame(Yes = pred, No = 1 - pred))
}

# Test predict_model() function
predict_model(x = model_keras, newdata = x_test_tbl, type = 'raw') %>%
  tibble::as_tibble()


# Run lime() on training set
explainer <- lime::lime(
  x              = x_train_tbl, 
  model          = model_keras, 
  bin_continuous = FALSE)

# Run explain() on explainer
explanation <- lime::explain(
  x_test_tbl[1:4,], 
  explainer    = explainer, 
  n_labels     = 1,                       # single class explanation
  n_features   = 4,                       # top 4 features
  kernel_width = 0.5)                     # increase "model_r2" value by shrinking the localized evaluation 

plot_features(explanation)
## feature importance plot
# green bar: feature supports the model conclusion
# red bar: contradict

## correlation analysis

# Feature correlations to Churn
corrr_analysis <- x_train_tbl %>%
  mutate(Churn = y_train_vec) %>%
  correlate() %>%
  focus(Churn) %>%
  rename(feature = rowname) %>%
  arrange(abs(Churn)) %>%
  mutate(feature = as_factor(feature)) 
corrr_analysis

