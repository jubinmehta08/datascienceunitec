
# Clear all objects
rm(list=ls())
par(mfrow = c(1, 1))      
cat("\14")

# Set Working directory
setwd("E:/ANZ - Data@ANZ")

#install.packages("ggpubr")

# Calling the libraries
library("dplyr")
library(stringr)
library(corrplot)
library("e1071")
library(lubridate)
library(rpart)
library(ggplot2)
library(ggpubr)
library(modelr)
library(boot) 

#############################################################
### Task 1 - Data Loading
#############################################################

# Read the Census data
myData <- read.csv("ANZ synthesised transaction dataset.csv", header = TRUE, sep = ',')
#myData <- read.csv("ANZ synthesised transaction dataset.csv", header = TRUE, na.strings=c("","NA"))


### Get the overall idea of dataset ###
str(myData)
summary(myData)
head(myData,10)

# Find the sum of all NULL values
sapply(myData,function(x)sum(is.na(x)))


#############################################################
##### Task 2 - Data Cleaning and Transformation #####
#############################################################

##### Function to clean data  #####
data_cleaning <- function(dtframe) {
  
  ### Removing irrelevant data, Removing all records with "?" ###
  dtframe[dtframe == "?"] <- NA
  #dtframe <- na.omit(dtframe)

  ## Changing the datatype to Factor
  dtframe$ï..status <- as.factor(dtframe$ï..status)
  dtframe$movement <- as.factor(dtframe$movement)
  dtframe$gender <- as.factor(dtframe$gender)
  
  ### Removing Outliers ###
  # Filtering the age, remove the age group less than 18 and more than 80
  dtframe <- subset(dtframe, !(dtframe$age >= 70 | dtframe$age < 15) )
  
  # Balance should be greater than "0" dollars
  dtframe <- subset(dtframe, !(dtframe$balance < 0) )
  
  
  ### Renaming Variables ###
  names(dtframe) = c("Status" , "Card_Present_Flag" , "Biller_Code" , "Account" , "Currency", "Long_Lat", "Transaction_desc", "Merchant_ID",
                     "Merchant_Code", "First_Name", "Balance", "date", "Gender", "Age", "Merchant_Suburb", 
                     "Merchant_State", "Extraction_Date", "Amount", "Transaction_ID", "Country", "Customer_ID", "Merchant_Long_Lat", "Movement")
  
  ### Removing the columns not necessary ###
  dtframe = select(dtframe, -Biller_Code, -Currency, -Long_Lat, -Transaction_desc, -Merchant_ID, -Merchant_Code, -Country, -Transaction_ID, -Merchant_Long_Lat)
  
  ### Finally, return the dataframe ###
  return(dtframe)
}

### Calling the Function ###
clean_data = data_cleaning(myData)

# Converting "Extraction_Date" into the date format
clean_data$Extraction_Date <- as.Date(clean_data$Extraction_Date)

#changing date column's format
clean_data$date <- as.Date(clean_data$date, format = "%d-%m-%Y")


str(clean_data)
View(clean_data)
summary(clean_data)


#############################################################
##### Task 3 - Univariate Analysis #####
#############################################################

########### EDA - Exploratory Data Analysis ############

### Examine Univariate data ##
### Below are the steps we will follow for all variables
### 1) explore discrete vs continuous variables
### 2) Two things to discover in EDA: central tendency, spread (outliers)


### 1st variable - Status
summary(clean_data$Status)
# Plotting of column - Age
plot(clean_data$Status, xlab = "Status", ylab = "Freqeuncy", main = "Status of Transaction", col = "green")

### 2nd variable - Movement
plot(clean_data$Movement, xlab = "Movement", ylab = "Freqeuncy", main = "Type of Transaction", col = "blue")

### 3rd variable - Age
boxplot(clean_data$Age, col = "blue", main = "Boxplot of Age")

### 4th Variable - Pie chart of Merchant State
pie(table(clean_data$Merchant_State), main = "Merchant State")

### 5th variable - Amount
hist(clean_data$Amount, col = "green", xlab = "Amount Withdrawn", main = "Histogram of Amount")

#------------------------------------------------------------------------------------------------


# Find the count of Date by transactions
table(clean_data$date)


#############################################################
##### Task 4 - Bivariate / Multiple variable Analysis #####
#############################################################

## Gender group transactions
gender_customer_count <- clean_data %>% 
  group_by(Gender) %>% 
  summarise(Count=n())

ggplot(gender_customer_count,aes(x=Gender,y=Count))+
  geom_col(fill="salmon")+geom_text(aes(label=Count),vjust=-0.2)+
  labs(title="Transactions",x="Gender",y="Total Transactions")


## Age group transactions
Age_Transactions <- clean_data %>% 
  mutate(Age_Category=case_when(18<=Age & Age<=20~'18-20', 
                                20<Age & Age<=30~'20-30',
                                30<Age & Age<=40~'30-40',
                                40<Age & Age<=50~'40-50',
                                50<Age & Age<=60~'50-60',
                      TRUE~'60-80')) %>% 
  group_by(Age_Category) %>% 
  summarise(Count=n())

ggplot(Age_Transactions,aes(x=Age_Category,y=Count))+geom_bar(fill="orange",width=0.3,stat="identity")+ggtitle("Age Distribution: ANZ Customer Transactions Database") + 
  ylab("No of Customers") + xlab("Age Category")

## Movement / Transaction types
plot(clean_data$Movement, xlab = "Movement", ylab = "Freqeuncy", main = "Transaction Types", col = "dark green")


## Merchant_state distribution
merchant_state_transactions <- clean_data %>% 
  filter(Merchant_State!="") %>% 
  group_by(Merchant_State) %>% 
  summarise(count=n())

ggplot(merchant_state_transactions,aes(x=Merchant_State,y=count))+geom_col(fill="light green")+geom_text(aes(Merchant_State,count+0.5,label=count))+
  labs(title="Total Transactions In Each Merchant State",x="State",y="Number of Transactions")


plot( clean_data$Age, clean_data$Balance, xlab="Age", ylab="Balance")


## Age and Amount relationship
plot( clean_data$Age, clean_data$Amount,
      xlab="Age", ylab="Amount", col=c("red","green"))


## Monthly transactions amount
month_trans_count <- clean_data %>% 
  mutate(month = 
           case_when(date >= "2018-08-01" & date <= "2018-08-31" ~ "August",
                     date >= "2018-09-01" & date <= "2018-09-30" ~ "September",
                     date >= "2018-10-01" & date <= "2018-10-31" ~ "October")) %>% 
  group_by(month) %>% 
  summarise(Count=sum(Amount))

ggplot(month_trans_count,aes(x=month,y=Count))+
  geom_col(fill="salmon")+geom_text(aes(label=Count),vjust=-0.2)+
  labs(title="Monthly Transactions",x="Month",y="Total Transactions")


## Monthly transactions based on Customer_ID
df2 <- clean_data %>%
  group_by(Customer_ID) %>%
  summarise(mon_avg_vol = round(n()/3,0))

hist(df2$mon_avg_vol, xlab= 'Monthly transaction volume', ylab='No. of customers', main = "Histogram of customers' monthly transaction volume")


#############################################################
##### Task - Our Analysis #####
#############################################################

Daterange <- seq(min(clean_data$date), max(clean_data$date),by=1)
Daterange[!Daterange %in% clean_data$Date]


