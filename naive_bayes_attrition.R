
rm(list = ls())

load_lb <- function()
{
  suppressPackageStartupMessages(library(doMC))
  registerDoMC(cores = 8)
  suppressPackageStartupMessages(library(readxl))
  suppressPackageStartupMessages(library(tidyr))
  suppressPackageStartupMessages(library(dplyr))
  suppressPackageStartupMessages(library(caret))
  suppressPackageStartupMessages(require(h2o))
  suppressPackageStartupMessages(require(data.table))
  suppressPackageStartupMessages(require(Matrix))
  suppressPackageStartupMessages(require(rsample))
}

load_lb()

## Attrition data: watson analytics sample data

df <- read_excel("E:\\Study\\R Projects\\Common files/Employee_Attrition.xlsx")

glimpse(df)
# 1470 X 35

# checking for NAs or blanks

sort(sapply(df, function(x) sum(is.na(x))))
# no NA

sort(sapply(df, function(x) sum(x == "")))
# no blanks

# column names
col_nm <- colnames(df)

# column types
spl <- split(col_nm,sapply(df, function(x) class(x)))

chr_col <- spl[[1]]
num_col <- spl[[2]]

# see and convert to factor

unique(df$JobLevel)                         #5 levels
unique(df$StockOptionLevel)                 #4 levels
unique(df$TrainingTimesLastYear)            #7 levels

df <- df %>% 
  mutate(JobLevel = factor(JobLevel),
         StockOptionLevel = factor(StockOptionLevel),
         TrainingTimesLastYear = factor(TrainingTimesLastYear))
glimpse(df)

# creating data splits as train and test
# I am using resample package, but we can use 'caret', 'H2O', base etc.

set.seed(931992)

splt <- initial_split(df, prop = 0.7, strata = "Attrition") #stratified sampling
train <- training(splt)
test <- testing(splt)


#distribution

train %>% 
  ggplot(aes(x = Attrition))+
  geom_histogram(stat = "count")

test %>% 
  ggplot(aes(x = Attrition))+
  geom_histogram(stat = "count")

table(train$Attrition) %>% prop.table()
table(test$Attrition) %>% prop.table()
# same proportion

# assumption of independece of variables (NB)

train %>% 
  select_if(is.numeric) %>% 
  cor() %>% 
  corrplot::corrplot()
  
## there seems correlation

# assumption of normality (continuous variable)
# so that the probability from Prob. density function can be used

num_col
check <- c("Age","DailyRate","DistanceFromHome","MonthlyRate","MonthlyIncome","HourlyRate")

train %>% 
  select(check) %>% 
  gather("metric", "value") %>% 
  ggplot(aes(value, fill = metric))+
  geom_density(show.legend = FALSE)+
  facet_wrap(~metric, scales = "free")

# box-cox transformation or non-parametric kernel density estimators

# feature creations

y <- train$Attrition
rest_features <- setdiff(names(train), "Attrition")
x <- train[,rest_features]

# 10 fold CV

train_c <- trainControl(method = "cv", number = 10)

naive_b <- caret::train(
  x = x,
  y = y,
  method = "nb",
  trControl = train_c
)

confusionMatrix(naive_b)

# tuning the parameters

search_g <- expand.grid(
  usekernel = c(TRUE, FALSE),
  fL = 0:5,
  adjust = seq(0,5, by = 1)
)

naive_b1 <- caret::train(
  x = x,
  y = y,
  method = "nb",
  trControl = train_c,
  tuneGrid = search_g,
  preProc = c("BoxCox","center","scale","pca")
)

pred <- predict(naive_b, newdata = test)
confusionMatrix(pred, factor(test$Attrition))

# very low specificity


  
