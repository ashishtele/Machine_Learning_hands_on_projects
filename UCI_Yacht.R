rm(list = ls())


library(mlr)
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
  library(parallelMap)
}

load_lb()
View(listLearners())

# Loading the input file

df <- fread("E:\\Study\\R Projects\\Common files\\yacht.txt",
            sep = " ",
            col.names = c('LongPos_COB', 'Prismatic_Coeff',
                          'Len_Disp_Ratio', 'Beam_Draut_Ratio', 
                          'Length_Beam_Ratio','Froude_Num', 
                          'Residuary_Resist'),
            data.table = FALSE)

class(df)
## Column names from data dictionary

glimpse(df)
# Dimension: 308 X 7

# No NA or blanks
sapply(df, function(x) sum(is.na(x)))
sapply(df, function(x) sum(x==""))


# Checking correlation

corr <- cor(df)
corrplot::corrplot(corr, 
                   method = "ellipse",
                   type = "upper")


# plotting the trend

names(df)

ggplot(df, aes(x=LongPos_COB, fill = "red")) +
  geom_density(show.legend = FALSE)

ggplot(df, aes(x=Prismatic_Coeff, fill = "red")) +
  geom_density(show.legend = FALSE)

ggplot(df, aes(x=Len_Disp_Ratio, fill = "red")) +
  geom_density(show.legend = FALSE)

ggplot(df, aes(x=Beam_Draut_Ratio, fill = "red")) +
  geom_density(show.legend = FALSE)

ggplot(df, aes(x=Length_Beam_Ratio, fill = "red")) +
  geom_density(show.legend = FALSE)

ggplot(df, aes(x=Froude_Num, fill = "red")) +
  geom_density(show.legend = FALSE)

ggplot(df, aes(x=Residuary_Resist, fill = "red")) +
  geom_density(show.legend = FALSE)

# Bi-variate analysis

ggplot(df,aes(x=Froude_Num, y = Residuary_Resist, color = "red")) +
  geom_point()
 
# scaling the data

scal <- function(x)
{
  return((x - min(x))/(max(x) - min(x)))
}

df <- scal(df)

# split the dataset

index <- createDataPartition(df$Residuary_Resist, p = 0.8, list = FALSE)
train <- df[index,]
test <- df[-index,]

# parallel start
parallelStartSocket(8)


# create a task
trainTask <- makeRegrTask(data = train,
                          target = "Residuary_Resist")
trainTask

rf <- makeLearner("regr.lm", 
                  predict.type = "response")
rf$par.vals
getParamSet(rf)


# Cross validation - resampling
rd <- makeResampleDesc("CV",iters = 5L)

# tune the hyperparameters
stune <- mlr::train(rf,trainTask)
names(stune)

pred <- predict(stune, newdata = test[,-7])
pred$data


rmse <- function(x,y)
{
  sqrt(sum((x - y)^2))
}

# rmse
rmse(test$Residuary_Resist,pred$data$response)


## Regression ANN
library(h2o)
h2o.init()


train.h2o <- as.h2o(train)
test.h2o <- as.h2o(test)

colnames(train.h2o)

# dependent variable
y.dep <- 7
x.ind <- c(1:6)


reg.h2o <- h2o.glm(y = y.dep,
                   x = x.ind,
                   training_frame = train.h2o,
                   family = "gaussian")                         # for - regression
h2o.performance(reg.h2o)

pred1 <- as.data.frame(h2o.predict(reg.h2o, test.h2o))
rmse(test$Residuary_Resist,pred1)


# GMB

h2o.gbm <- h2o.gbm(y = y.dep,
                   x = x.ind,
                   training_frame = train.h2o,
                   ntrees = 100,
                   max_depth = 4,
                   learn_rate = 0.01,
                   seed = 123)
h2o.gbm
pred2 <- as.data.frame(h2o.predict(h2o.gbm, test.h2o))
rmse(test$Residuary_Resist, pred2)


# DLearning model

h2o.dl <- h2o.deeplearning(y = y.dep,
                           x = x.ind,
                           training_frame = train.h2o,
                           epochs = 100,
                           hidden = c(6,1),
                           activation = "Rectifier",
                           seed = 123)
h2o.dl
pred3 <- as.data.frame(h2o.predict(h2o.dl,test.h2o))
rmse(test$Residuary_Resist, pred3)

SSE <- function(x,y)
{
  sum((x - y)^2)/2
}

SSE(test$Residuary_Resist, pred3)
