#Importing the Bionmore sales
library(readxl)
BinmoreSales <- read_excel("E:/Basics of Statistics/Assign 1/BinmoreSales.xlsx")

#Task 1 : classify the variables

str(BinmoreSales)
class(BinmoreSales)

#Task 2.1 : Central tendency of all transactions

BinmoreSales_mean <- mean(BinmoreSales$Amount)
BinmoreSales_median <- median(BinmoreSales$Amount)
BinmoreSales_sd <- sd(BinmoreSales$Amount)

#Task 2.2 : Central tendenct of amount < $100

BinmoreSales_lt_100 <- subset(BinmoreSales, Amount < 100) #OR
D_lt_100 <- BinmoreSales$Amount[BinmoreSales$Amount < 100]
D_mean <- mean(D_lt_100)
D_median <- median(D_lt_100)
D_sd <- sd(D_lt_100)

#Task 3 : Z-Score calculation

BinmoreSales$z_num <- BinmoreSales$Amount - BinmoreSales_mean
BinmoreSales$z_scores <- BinmoreSales$z_num/BinmoreSales_sd

Outlier_trans_cd <- BinmoreSales$`Transaction Code`[BinmoreSales$z_scores < -3 | BinmoreSales$z_scores > 3]
Outlier_trans_cd

#Task 4

#Region with Most Transactions
BinmoreSales$cnt <- 1
BinmoreSales_aggr <- aggregate(cnt ~ Region, data = BinmoreSales, FUN = sum)
BinmoreSales_aggr_srt <- BinmoreSales_aggr[order(-BinmoreSales_aggr$cnt),]
head(BinmoreSales_aggr_srt,1)

#Region with highest Mean payment amount
BinmoreSales_high_mean <- aggregate(Amount ~ Region, data = BinmoreSales, FUN =mean)
BinmoreSales_h_mean_srt <- BinmoreSales_high_mean[order(-BinmoreSales_high_mean$Amount),]
head(BinmoreSales_h_mean_srt,1)

#Region with highest SD in Payment amount
BinmoreSales_high_sd <- aggregate(Amount ~ Region, data = BinmoreSales, FUN =sd)
BinmoreSales_h_sd_srt <- BinmoreSales_high_sd[order(-BinmoreSales_high_sd$Amount),]
head(BinmoreSales_h_sd_srt,1)

#Task 5

T <- sort(BinmoreSales$Time, decreasing = FALSE)
i <- 1
T_new <- 0
while(i < nrow(BinmoreSales))
{
  T_new[i] <- T[i+1] - T[i]
  i=i+1
}

T_minute <- T_new*60
T_mean <- mean(T_minute)
T_mean
T_sd <- sd(T_minute)
T_sd


#Task 6:

length(BinmoreSales$Payment[BinmoreSales$Payment == "Paypal"])/length(BinmoreSales$Payment[BinmoreSales$Payment == "Credit"])
#OR
nrow(subset(BinmoreSales, Payment == "Paypal"))/nrow(subset(BinmoreSales, Payment == "Credit"))

length(BinmoreSales$Source[BinmoreSales$Source == "Web"])/length(BinmoreSales$Source[BinmoreSales$Source == "Email"])
#OR
nrow(subset(BinmoreSales, Source == "Web"))/nrow(subset(BinmoreSales, !(Source == "Web")))

length(BinmoreSales$Product[BinmoreSales$Product == "Book"])/length(BinmoreSales$Product[BinmoreSales$Product == "DVD"])
#OR
nrow(subset(BinmoreSales, Product == "Book"))/nrow(subset(BinmoreSales, Product == "DVD"))



