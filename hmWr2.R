#Importing Bestdeal data file
library(readxl)
BD <- read_excel("E:\\Basics of Statistics\\Assign 2/Bestdeals.xlsx", 
                 col_types = c("text","numeric","numeric","numeric","numeric",
                               "numeric","numeric","numeric","numeric","numeric","numeric"))

##Sales vector creation
Sales.Vector <- as.vector(unlist(BD[-c(1)]))
#removed date column
length(Sales.Vector[Sales.Vector=="NA"])
#All the 10 employees have taken 10 days leaves

##Compute the statictics
BD_mean <- mean(Sales.Vector, na.rm = TRUE)
BD_mdian <- median(Sales.Vector, na.rm = TRUE)
BD_range <- range(Sales.Vector, na.rm = TRUE)
BD_sd <- sd(Sales.Vector, na.rm = TRUE)
Cof_var <- BD_sd/BD_mean

library(moments)
BD_skew <- skewness(Sales.Vector, na.rm = TRUE)
BD_kurt <- kurtosis(Sales.Vector, na.rm = TRUE)

##tapply function
library(tidyr)
BD_gather <- gather(BD, key = "Name", value = "Sales", -Day)
BD_no_na <- na.omit(BD_gather)

Emp_mean <- tapply(BD_no_na$Sales, BD_no_na$Name, mean)
Emp_mean
Emp_sd <- tapply(BD_no_na$Sales, BD_no_na$Name, sd)
Emp_sd
Emp_cv <- Emp_sd/Emp_mean
Emp_cv
Employee.Summary <- data.frame(Emp_mean,Emp_sd,Emp_cv)
Employee.Summary

##High and Low performance
Employee.Summary$performace <- ifelse(Employee.Summary$Emp_mean-0.2*Employee.Summary$Emp_sd>146,"High",
                                      ifelse(Employee.Summary$Emp_mean+0.2*Employee.Summary$Emp_sd<146,"Low","NA"))


##Daily Sales calculation
Avg_sales <- tapply(BD_gather$Sales, BD_gather$Day, mean, na.rm = TRUE)
Avg_sales
BD_na <- subset(BD_gather, is.na(Sales))
No_abs <- tapply(is.na(BD_gather$Sales), BD_gather$Day, sum, na.rm = TRUE)
No_abs
Daily.Sales <- data.frame(Avg_sales,No_abs)
head(Daily.Sales)
