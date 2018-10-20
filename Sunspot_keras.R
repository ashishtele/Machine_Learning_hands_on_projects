

####################################################################################
############################ LSTM sunsplot - Keras #################################
####################################################################################

rm(list = ls())


library(tidyquant)
library(tidyverse)
library(timetk)
library(tibbletime)
library(recipes)
library(rsample)
library(yardstick)
library(keras)

# data load


class(sunspot.month)   # ts
frequency(sunspot.month)  # 12 - Monthly
cycle(sunspot.month)
time(sunspot.month)
deltat(sunspot.month)

sun_spot <- sunspot.month %>% 
  tk_tbl() %>%                              # ts to table and preserve the Index
  mutate(index = as_date(index)) %>% 
  as_tbl_time(index = index)                # column to be used for time-based subsetting

glimpse(sun_spot)
class(sun_spot)


# exploratory data analysis

p1 <- sun_spot %>% 
  ggplot(aes(index, value))+
  geom_point(color = palette_green()[[1]])+
  theme_tq()
p1

p2 <- sun_spot %>% 
  filter_time("start" ~ "1800") %>%             # from start to 1800
  ggplot(aes(index, value))+
  geom_line(color = palette_light()[[1]])+
  geom_point(color = palette_light()[[1]])+
  geom_smooth(method = "loess", span = 0.2, se=FALSE)+
  theme_tq()
p2

gridExtra::grid.arrange(p1,p2,ncol = 1)


# check whether or not to use LSTM model

## batch forecast: single forecast batch across the forecast region
## batch prediction works if autocorrelation used is beyond 10 years


acf(sun_spot)
library(forecast)
Acf(sun_spot$value, lag.max = 150) %>% 
  autoplot()

# Backtesting strategy

period_train <- 12 * 100
period_test <- 12 * 50
skip_span <- 12 * 22 - 1

roll_origin <- rolling_origin(sun_spot,
                              initial = period_train,
                              assess = period_test,
                              cumulative = FALSE,
                              skip = skip_span)
roll_origin

# visualizing the backtesting

plot_split <- function(split, alpha = 1, size = 1, base_size = 14) {
  
  # Manipulate data
  train_tbl <- training(split) %>%
    add_column(key = "training") 
  
  test_tbl  <- testing(split) %>%
    add_column(key = "testing") 
  
  data_manipulated <- bind_rows(train_tbl, test_tbl) %>%
    as_tbl_time(index = index) %>%
    mutate(key = fct_relevel(key, "training", "testing"))
  
  # Collect attributes
  train_time_summary <- train_tbl %>%
    tk_index() %>%
    tk_get_timeseries_summary()
  
  test_time_summary <- test_tbl %>%
    tk_index() %>%
    tk_get_timeseries_summary()
  
  # Visualize
  g <- data_manipulated %>%
    ggplot(aes(x = index, y = value, color = key)) +
    geom_line(size = size, alpha = alpha) +
    theme_tq(base_size = base_size) +
    scale_color_tq() +
    theme(legend.position = "none") 
  
  return(g)
}

roll_origin$splits[[1]] %>% 
  plot_split()


# modeling the keras stateful LSTM Model

## single LSTM

split <- roll_origin$splits[[6]]
split_id <- roll_origin$id[[6]]

roll_origin$splits[[6]] %>% 
  plot_split()

# Data setup

df_train <- training(split)
df_trn <- df_train[1:800,]
df_val <- df_train[801:1200,]
df_test <- testing(split)

df <- bind_rows(
  df_trn %>% add_column(key = "training"),
  df_val %>% add_column(key = "validation"),
  df_test %>% add_column(key = "testing")
) %>% 
  as_tbl_time(index = index)

df

# pre-processing (recipes)
## center and scaled


rec_obj <- recipe(value ~., df) %>% 
  step_sqrt(value) %>% 
  step_center(value) %>% 
  step_scale(value) %>% 
  prep()

df_prp_tbl <- bake(rec_obj, df)
df_prp_tbl

# capturing history to revert

center_history <- rec_obj$steps[[2]]$means["value"]
scale_history <- rec_obj$steps[[3]]$sds["value"]

# LSTM PLAN

# predictor must be 3D (sample, timesteps, features)
# target must be 2D (sample, timesteps)
# train and test length must be evenly divisible
# batch size must be divisible into both training and testing (ratio be whole number)

n_timesteps <- 12
n_predictions <- n_timesteps
batch_size <- 10

# functions used
build_matrix <- function(tseries, overall_timesteps) 
  {
  t(sapply(1:(length(tseries) - overall_timesteps + 1), function(x) 
    tseries[x:(x + overall_timesteps - 1)]))
}

reshape_X_3d <- function(X) {
  dim(X) <- c(dim(X)[1], dim(X)[2], 1)
  X
}


# extract values from data frame
train_vals <- df_prp_tbl %>%
  filter(key == "training") %>%
  select(value) %>%
  pull()
valid_vals <- df_prp_tbl %>%
  filter(key == "validation") %>%
  select(value) %>%
  pull()
test_vals <- df_prp_tbl %>%
  filter(key == "testing") %>%
  select(value) %>%
  pull()



# build the windowed matrices
train_matrix <-
  build_matrix(train_vals, n_timesteps + n_predictions)
valid_matrix <-
  build_matrix(valid_vals, n_timesteps + n_predictions)
test_matrix <- 
  build_matrix(test_vals, n_timesteps + n_predictions)



X_train <- train_matrix[, 1:n_timesteps]
y_train <- train_matrix[, (n_timesteps + 1):(n_timesteps * 2)]
X_train <- X_train[1:(nrow(X_train) %/% batch_size * batch_size), ]
y_train <- y_train[1:(nrow(y_train) %/% batch_size * batch_size), ]

X_valid <- valid_matrix[, 1:n_timesteps]
y_valid <- valid_matrix[, (n_timesteps + 1):(n_timesteps * 2)]
X_valid <- X_valid[1:(nrow(X_valid) %/% batch_size * batch_size), ]
y_valid <- y_valid[1:(nrow(y_valid) %/% batch_size * batch_size), ]

X_test <- test_matrix[, 1:n_timesteps]
y_test <- test_matrix[, (n_timesteps + 1):(n_timesteps * 2)]
X_test <- X_test[1:(nrow(X_test) %/% batch_size * batch_size), ]
y_test <- y_test[1:(nrow(y_test) %/% batch_size * batch_size), ]
# add on the required third axis
X_train <- reshape_X_3d(X_train)
X_valid <- reshape_X_3d(X_valid)
X_test <- reshape_X_3d(X_test)

y_train <- reshape_X_3d(y_train)
y_valid <- reshape_X_3d(y_valid)
y_test <- reshape_X_3d(y_test)

## Model building
FLAGS <- flags(
  # There is a so-called "stateful LSTM" in Keras. While LSTM is stateful
  # per se, this adds a further tweak where the hidden states get 
  # initialized with values from the item at same position in the previous
  # batch. This is helpful just under specific circumstances, or if you want
  # to create an "infinite stream" of states, in which case you'd use 1 as 
  # the batch size. Below, we show how the code would have to be changed to
  # use this, but it won't be further discussed here.
  flag_boolean("stateful", FALSE),
  # Should we use several layers of LSTM?
  # Again, just included for completeness, it did not yield any superior 
  # performance on this task.
  # This will actually stack exactly one additional layer of LSTM units.
  flag_boolean("stack_layers", FALSE),
  # number of samples fed to the model in one go
  flag_integer("batch_size", 10),
  # size of the hidden state, equals size of predictions
  flag_integer("n_timesteps", 12),
  # how many epochs to train for
  flag_integer("n_epochs", 100),
  # fraction of the units to drop for the linear transformation of the inputs
  flag_numeric("dropout", 0.2),
  # fraction of the units to drop for the linear transformation of the 
  # recurrent state
  flag_numeric("recurrent_dropout", 0.2),
  # loss function. Found to work better for this specific case than mean
  # squared error
  flag_string("loss", "logcosh"),
  # optimizer = stochastic gradient descent. Seemed to work better than adam 
  # or rmsprop here (as indicated by limited testing)
  flag_string("optimizer_type", "sgd"),
  # size of the LSTM layer
  flag_integer("n_units", 128),
  # learning rate
  flag_numeric("lr", 0.003),
  # momentum, an additional parameter to the SGD optimizer
  flag_numeric("momentum", 0.9),
  # parameter to the early stopping callback
  flag_integer("patience", 10)
)

# the number of predictions we'll make equals the length of the hidden state
n_predictions <- FLAGS$n_timesteps
# how many features = predictors we have
n_features <- 1
# just in case we wanted to try different optimizers, we could add here
optimizer <- switch(FLAGS$optimizer_type,
                    sgd = optimizer_sgd(lr = FLAGS$lr, 
                                        momentum = FLAGS$momentum)
)

# callbacks to be passed to the fit() function
# We just use one here: we may stop before n_epochs if the loss on the
# validation set does not decrease (by a configurable amount, over a 
# configurable time)
callbacks <- list(
  callback_early_stopping(patience = FLAGS$patience)
)

# create the model
model <- keras_model_sequential()

# add layers
# we have just two, the LSTM and the time_distributed 
model %>%
  layer_lstm(
    units = FLAGS$n_units, 
    # the first layer in a model needs to know the shape of the input data
    batch_input_shape  = c(FLAGS$batch_size, FLAGS$n_timesteps, n_features),
    dropout = FLAGS$dropout,
    recurrent_dropout = FLAGS$recurrent_dropout,
    # by default, an LSTM just returns the final state
    return_sequences = TRUE
  ) %>% time_distributed(layer_dense(units = 1))

model %>%
  compile(
    loss = FLAGS$loss,
    optimizer = optimizer,
    # in addition to the loss, Keras will inform us about current 
    # MSE while training
    metrics = list("mean_squared_error")
  )

history <- model %>% fit(
  x          = X_train,
  y          = y_train,
  validation_data = list(X_valid, y_valid),
  batch_size = FLAGS$batch_size,
  epochs     = FLAGS$n_epochs,
  callbacks = callbacks
)


plot(history, metrics = "loss")                  # overfitting

pred_train <- model %>%
  predict(X_train, batch_size = FLAGS$batch_size) %>%
  .[, , 1]

# Retransform values to original scale
pred_train <- (pred_train * scale_history + center_history) ^2
compare_train <- df %>% filter(key == "training")

# build a dataframe that has both actual and predicted values
for (i in 1:nrow(pred_train)) {
  varname <- paste0("pred_train", i)
  compare_train <-
    mutate(compare_train,!!varname := c(
      rep(NA, FLAGS$n_timesteps + i - 1),
      pred_train[i,],
      rep(NA, nrow(compare_train) - FLAGS$n_timesteps * 2 - i + 1)
    ))
}

coln <- colnames(compare_train)[4:ncol(compare_train)]
cols <- map(coln, quo(sym(.)))
rsme_train <-
  map_dbl(cols, function(col)
    rmse(
      compare_train,
      truth = value,
      estimate = !!col,
      na.rm = TRUE
    )) %>% mean()

rsme_train

ggplot(compare_train, aes(x = index, y = value)) + geom_line() +
  geom_line(aes(y = pred_train1), color = "cyan") +
  geom_line(aes(y = pred_train50), color = "red") +
  geom_line(aes(y = pred_train100), color = "green") +
  geom_line(aes(y = pred_train150), color = "violet") +
  geom_line(aes(y = pred_train200), color = "cyan") +
  geom_line(aes(y = pred_train250), color = "red") +
  geom_line(aes(y = pred_train300), color = "red") +
  geom_line(aes(y = pred_train350), color = "green") +
  geom_line(aes(y = pred_train400), color = "cyan") +
  geom_line(aes(y = pred_train450), color = "red") +
  geom_line(aes(y = pred_train500), color = "green") +
  geom_line(aes(y = pred_train550), color = "violet") +
  geom_line(aes(y = pred_train600), color = "cyan") +
  geom_line(aes(y = pred_train650), color = "red") +
  geom_line(aes(y = pred_train700), color = "red") +
  geom_line(aes(y = pred_train750), color = "green") +
  ggtitle("Predictions on the training set")







