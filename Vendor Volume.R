######### Library loading

library(readxl)
library(dplyr)

### data read

data = read.csv("E:\\Capstone\\OneDrive_2018-08-29\\Data Files For Fall 2018 Capstone\\BSAK_BKPF_AltColTitles.csv")
glimpse(data)

## Data exploration

names(data)
## Business rule 1: Document type <> ZP or KZs

data %>% 
  filter(!(Document_Type %in% c('ZP','KZ'))) %>% 
  select(Account_Number_of_Vendor_or_Creditor, Document_Number_of_the_Clearing_Document,
         Amount_in_Local_Currency, Reference_Document_Number, Document_Type) %>% 
  mutate(Reference_Doc_1 = if_else(nchar(as.character(Reference_Document_Number))==0,"Blank",as.character(Reference_Document_Number)),
         Reference_Document_Number = as.character(Reference_Document_Number),
         Reference_Doc_1 = as.character(Reference_Doc_1),
         Document_type_1 = if_else(nchar(as.character(Document_Type))==0,"Blank",as.character(Document_Type))) -> data_1

names(data_1)

write.csv2(data_1,"E:\\Capstone\\OneDrive_2018-08-29\\Data Files For Fall 2018 Capstone\\que5.csv")

library(xlsx)
write.xlsx(data_1, "E:\\Capstone\\OneDrive_2018-08-29\\Data Files For Fall 2018 Capstone\\que5.xlsx")

## Invoice level aggregate - total amount

library(ggplot2)

data_1 %>% 
  group_by(Reference_Doc_1) %>% 
  summarise(no_trans = n(),
            tot_amt = sum(Amount_in_Local_Currency),
            avg_amt = mean(Amount_in_Local_Currency)) %>% 
  arrange(-tot_amt) %>% 
  top_n(10, tot_amt) %>% 
  ggplot(aes(reorder(Reference_Doc_1,tot_amt), tot_amt, fill = Reference_Doc_1)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  scale_y_continuous(labels=scales::dollar_format(prefix="$"))+
  coord_flip()+
  labs(x = "Reference Doc",
            y = "curr")

## Invoice level aggregate - transactions amount

library(ggplot2)

data_1 %>% 
  group_by(Reference_Doc_1) %>% 
  summarise(no_trans = n(),
            tot_amt = sum(Amount_in_Local_Currency),
            avg_amt = mean(Amount_in_Local_Currency)) %>% 
  arrange(-no_trans) %>% 
  top_n(10, no_trans) %>% 
  ggplot(aes(reorder(Reference_Doc_1,no_trans), no_trans, fill = Reference_Doc_1)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  #scale_y_continuous(labels=scales::dollar_format(prefix="$"))+
  coord_flip()+
  labs(x = "Reference Doc",
       y = "No. of Transactions")

# Vendor level analysis

data_1 %>% 
  group_by(Account_Number_of_Vendor_or_Creditor = as.character(Account_Number_of_Vendor_or_Creditor)) %>% 
  summarise(no_trans = n(),
            tot_amt = sum(Amount_in_Local_Currency),
            avg_amt = mean(Amount_in_Local_Currency)) %>% 
  arrange(-no_trans) %>% 
  top_n(10, no_trans) %>% 
  ggplot(aes(reorder(as.character(Account_Number_of_Vendor_or_Creditor),no_trans), no_trans, fill = as.character(Account_Number_of_Vendor_or_Creditor))) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  #scale_y_continuous(labels=scales::dollar_format(prefix="$"))+
  coord_flip()+
  labs(x = "Vendor",
       y = "No. of Transactions")

data_1 %>% 
  group_by(Account_Number_of_Vendor_or_Creditor = as.character(Account_Number_of_Vendor_or_Creditor)) %>% 
  summarise(no_trans = n(),
            tot_amt = sum(Amount_in_Local_Currency),
            avg_amt = mean(Amount_in_Local_Currency)) %>% 
  arrange(-tot_amt) %>% 
  top_n(10, tot_amt) %>% 
  ggplot(aes(reorder(as.character(Account_Number_of_Vendor_or_Creditor),avg_amt), avg_amt, fill = as.character(Account_Number_of_Vendor_or_Creditor))) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  scale_y_continuous(labels=scales::dollar_format(prefix="$"))+
  coord_flip()+
  labs(x = "Vendor",
       y = "Average Amount")
