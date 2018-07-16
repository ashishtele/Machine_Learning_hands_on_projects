
rm(list = ls())

# Loading the libraries
load_lb <- function()
{
  suppressPackageStartupMessages(library(doMC))
  registerDoMC(cores = 8)
  suppressPackageStartupMessages(library(readxl))
  suppressPackageStartupMessages(library(tidyr))
  suppressPackageStartupMessages(library(dplyr))
  suppressPackageStartupMessages(library(caret))
  suppressPackageStartupMessages(library(rpart))
  suppressPackageStartupMessages(library(tree))
  suppressPackageStartupMessages(library(MASS))
  suppressPackageStartupMessages(library(mice))
  suppressPackageStartupMessages(require(xgboost))
  suppressPackageStartupMessages(require(data.table))
  suppressPackageStartupMessages(require(Matrix))
  library(mlr)
}

load_lb()


# Importing the input files

train <- fread("E:\\Study\\R Projects\\Common files\\Purchase_train.csv",
               stringsAsFactors = TRUE)
test <- fread("E:\\Study\\R Projects\\Common files\\Purchase_test.csv",
              stringsAsFactors = TRUE)

glimpse(train)
# 550,068 X 12
glimpse(test)
# 233,599 X 11

summarizeColumns(train)
summarizeColumns(test)

summary(train)
summary(test)

# As we are going to use H2O, we are seperating the dependent and other predictors

test %>% 
  mutate(Purchase = mean(train$Purchase)) -> test

df <- bind_rows(train, test)
glimpse(df)

### variable exploration

prop.table(table(df$Gender))
prop.table(table(df$Stay_In_Current_City_Years))

# Unique values in Product_id & User_ID

length(unique(df$Product_ID))
length(unique(df$User_ID))

# Missing values

sort(sapply(df, function(x) sum(is.na(x))), decreasing = TRUE)
sort(sapply(df, function(x) sum(x == "")), decreasing = TRUE)
## Product cat 3 and 2 are having NAs

split(names(df), sapply(df, function(x) class(x)))

unique(df$Gender)
unique(df$Age)            # 7 buckets
unique(df$City_Category)  # 3 categories
unique(df$Stay_In_Current_City_Years)  # 5 levels

## Age vs Gender
library(ggplot2)

ggplot(df, aes(Age, fill = Gender)) +
  geom_bar()

ggplot(df, aes(Age, fill = City_Category)) +
  geom_bar()
## Age bucket 26-35 has max users.

ggplot(df, aes(Stay_In_Current_City_Years, fill = City_Category)) +
  geom_bar()

ggplot(df, aes(Stay_In_Current_City_Years, fill = Gender)) +
  geom_bar()
## Most of users stay in current city for 1 year.

## Output variable (train)

qplot(train$Purchase)     # no particular trend
ggplot(train, aes(Purchase)) +
  geom_density()

### Data manipulation

# new variable creation

df %>% 
  mutate(pro_cat_2_na = ifelse(sapply(df$Product_Category_2,is.na) == TRUE,1,0),
         pro_cat_3_na = ifelse(sapply(df$Product_Category_3,is.na) == TRUE,1,0)) %>% 
  mutate(Product_Category_2 = ifelse(is.na(Product_Category_2) == TRUE, -992,Product_Category_2),
         Product_Category_3 = ifelse(is.na(Product_Category_3) == TRUE, -992,Product_Category_3)) -> df
glimpse(df)


levels(df$Stay_In_Current_City_Years)[5] <- "4"
str(df$Stay_In_Current_City_Years)

# ordering the age bucket

levels(df$Age)[1] <- 0
levels(df$Age)[2] <- 1
levels(df$Age)[3] <- 2
levels(df$Age)[4] <- 3
levels(df$Age)[5] <- 4
levels(df$Age)[6] <- 5
levels(df$Age)[7] <- 6

df %>% 
  mutate(Age = as.numeric(Age)) -> df

levels(df$Gender)
df$Gender <- plyr::revalue(df$Gender, c("F" = 0, "M" = 1))
glimpse(df)

df %>% 
  mutate(Gender = as.numeric(Gender),
         Stay_In_Current_City_Years = as.numeric(Stay_In_Current_City_Years)) -> df

df %>% 
  group_by(User_ID) %>% 
  mutate(user_cnt = n(),
         user_mean = mean(Purchase)) -> df

df %>% 
  group_by(Product_ID) %>% 
  mutate(Product_cnt = n(),
         Product_mean = mean(Purchase)) -> df

df$Product_ID <- as.factor(df$Product_ID)
  
dummy <- dummyVars(~ City_Category, data = df, fullRank = TRUE)
try <- predict(dummy, df) %>% as.data.frame()

df1 <- df
df1$City_Category.B <- try$City_Category.B
df1$City_Category.C <- try$City_Category.C
df <- df1
rm(df1)

df$City_Category <- NULL

## train and test data

train.h <- df[1:nrow(train),]
test.h <- df[-(1:nrow(train)),]

# removing the counts not present in test data
train.h$Product_Category_1[train.h$Product_Category_1 > 18]

train.h <- train.h[train.h$Product_Category_1 < 19, ]

# Loading H2O
library(h2o)

# Launching all clusters
h2o.init(nthreads = -1)

#h2o.init()

# h2o dataframe

train.h2o <- as.h2o(train.h)
test.h2o <- as.h2o(test.h)

# Column index number
colnames(train.h2o)

# Dependent variable (leaving user_id and product_id)
y_dep <- 11
# Predictors
x_ind <- c(3:10,12:19)

## Regression

reg <- h2o.glm(y = y_dep,
               x = x_ind,
               training_frame = train.h2o,
               family = "gaussian")
h2o.performance(reg)
pred.reg <- h2o.predict(reg, test.h2o)

## Random forest

rf <- h2o.randomForest(y = y_dep,
                       x = x_ind,
                       training_frame = train.h2o,
                       ntrees = 500,
                       mtries = 3,
                       max_depth = 4,
                       seed = 931992)
h2o.performance(rf)
h2o.varimp(rf)

# Custom function for R Square
Rsquare <- function(y,y_new)
{
  RSS <- sum((y-y_new)^2)
  TSS <- sum((y-mean(y))^2)
  return(1-RSS/TSS)
}
pred.rf <- h2o.predict(rf, train.h2o)
t <- as.data.frame(pred.rf)
pred.rf <- h2o.predict(rf, test.h2o)

Rsquare(train.h$Purchase,t$predict)


# GBM

pred.gbm <- h2o.gbm(y = y_dep,
                    x = x_ind,
                    training_frame = train.h2o,
                    ntrees = 900,
                    max_depth = 4,
                    learn_rate = 0.01,
                    seed = 931992)
h2o.performance(pred.gbm, newdata = test.h2o)


# GBM hyperparameters

tv <- h2o.splitFrame(train.h2o, seed = 1)
train.tv <- tv[[1]]
test.tv <- tv[[2]]

gbm_par <- list(learn_rate = c(0.01,0.1),
                max_depth = c(3,5,9),
                sample_rate = c(0.8,1.0),
                col_sample_rate = c(0.2,0.5,1.0))

gbm_grid <- h2o.grid("gbm", x = x_ind, y = y_dep,
                     grid_id = "gbm_grid",
                     training_frame = train.tv,
                     validation_frame = test.tv,
                     ntrees = 500,
                     seed = 1,
                     hyper_params = gbm_par)

gbm_gridperf <- h2o.getGrid(grid_id = "gbm_grid",
                            sort_by = "mse",
                            decreasing = FALSE)
print(gbm_gridperf)

best_gbm <- h2o.getModel(gbm_gridperf@model_ids[[1]])
pred.gbm1 <- h2o.performance(model = best_gbm,
                             newdata = test.h2o)
pred.gbm1
