
# Clear all objects
rm(list=ls())
par(mfrow = c(1, 1))      
cat("\14")

# Set Working directory
setwd("C:/Users/DELL/Desktop/Lectures/Intro to Data Science/Assignment 2")

# Calling the libraries
library("dplyr")
library(stringr)
library(corrplot)
library("e1071")


#############################################################
### Task 1 - Data Collection
#############################################################

# Read the Census data
myBankData <- read.csv("bank-additional.csv", header = TRUE, sep = ';')


### Get the overall idea of dataset ###
str(myBankData)
summary(myBankData)
head(myBankData)


#############################################################
##### Task 2 - Data Cleaning and Transformation #####
#############################################################

##### Function to clean data  #####
data_cleaning <- function(dtframe) {
  
  ### Removing irrelevant data, Removing all records with "?" ###
  dtframe[dtframe == "?"] <- NA
  dtframe <- na.omit(dtframe)
  
  # Mapping "unknown" values to different values of that columns, eg. "unemployed"
  dtframe$job <- as.character(dtframe$job)
  dtframe$job[dtframe$job == "unknown"] <- "unemployed"
  dtframe$job <- as.factor(dtframe$job)
  
  # Mapping "unknown" values to different values of that columns, eg. "single"
  dtframe$marital <- as.character(dtframe$marital)
  dtframe$marital[dtframe$marital == "unknown"] <- "single"
  dtframe$marital <- as.factor(dtframe$marital)
  
  # Mapping "unknown" values of "Education" columns to, eg. "illiterate"
  dtframe$education <- as.character(dtframe$education)
  dtframe$education[dtframe$education == "unknown"] <- "illiterate"
  dtframe$education <- as.factor(dtframe$education)
  
  
  # Mapping "unknown" values of "default" column to: eg. "yes"
  dtframe$default <- as.character(dtframe$default)
  dtframe$default[dtframe$default == "unknown"] <- "yes"
  dtframe$default <- as.factor(dtframe$default)
  
  # Mapping "unknown" values of "housing" column to: eg. "no"
  dtframe$housing <- as.character(dtframe$housing)
  dtframe$housing[dtframe$housing == "unknown"] <- "no"
  dtframe$housing <- as.factor(dtframe$housing)
  
  # Mapping "unknown" values of "loan" column to: eg. "yes"
  dtframe$loan <- as.character(dtframe$loan)
  dtframe$loan[dtframe$loan == "unknown"] <- "yes"
  dtframe$loan <- as.factor(dtframe$loan)
  
  
  # Mapping all relevant "day_of_week" column records
  dtframe$day_of_week <- as.character(dtframe$day_of_week)
  
  dtframe$day_of_week[dtframe$day_of_week == "mon"] <- "Monday"
  dtframe$day_of_week[dtframe$day_of_week == "tue"] <- "Tuesday"
  dtframe$day_of_week[dtframe$day_of_week == "wed"] <- "Wednesday"
  dtframe$day_of_week[dtframe$day_of_week == "thu"] <- "Thursday"
  dtframe$day_of_week[dtframe$day_of_week == "fri"] <- "Friday"
  
  dtframe$day_of_week <- as.factor(dtframe$day_of_week)
  
  
  ### Removing Outliers ###
  # Filtering the age, remove the age group less than 18 and more than 80
  dtframe <- subset(dtframe, !(dtframe$age >= 80 | dtframe$age < 18) )
  
  # Campaign days should be greater than "0" days
  dtframe <- subset(dtframe, !(dtframe$campaign < 1) )

  ### Encoding/Re-coding variables & Converting Units ###
  #dtframe$y <- as.character(dtframe$y)
  #dtframe$y[dtframe$y == "no"] <- "0"
  #dtframe$y[dtframe$y == "yes"] <- "1"
  
  # Converting seconds into minutes; mapping "seconds" into "minutes" for duration column
  dtframe$duration <- round(dtframe$duration/60, 3)
  
  
  # 999 means client was not previously contacted; mapping 999 to 0 for pdays column
  dtframe$pdays[dtframe$pdays == 999] <- 0
  
  
  ### Renaming Variables ###
  names(dtframe) = c("Age" , "Job" , "Marital" , "Education" , "Defaulter", "Housing_Loan", "Personal_Loan", "Contact",
                     "Month", "Weekday", "Duration", "Campaign_Days", "Prev_Days_Passed", "Prev_Camp_Days", "Prev_Outcome", 
                     "Emp_Var_Rate", "Consumer_Price_Ind", "Consumer_Conf_Ind", "Euri_3_month_Rate", "No_of_Employees", "Term_Deposit")
  
  
  ### Removing the columns not necessary ###
  #dtframe = select(dtframe, -Euri_3_month_Rate)
  
  
  ### Finally, return the dataframe ###
  return(dtframe)
}

### Calling the Function ###
clean_data = data_cleaning(myBankData)

str(clean_data)
View(clean_data)
summary(clean_data)


par(mfrow=c(2,2))


#############################################################
##### Task 3 - Univariate Analysis #####
#############################################################

########### EDA - Exploratory Data Analysis ############

### Examine Univariate data ##
### Below are the steps we will follow for all variables
### 1) explore discrete vs continuous variables
### 2) Two things to discover in EDA: central tendency, spread (outliers)

#----------------------------------
### 1st variable - Age
summary(clean_data$Age)


# Plotting of column - Age
boxplot(clean_data$Age, col = "blue", main = "Boxplot of Age")
dev.copy(png,"plot1.png")
dev.off()
hist(clean_data$Age, col = "dark green", xlab = "Age", main = "Histogram of Age")
plot(density(clean_data$Age), main="Density Spread")


# Other Values, Quantitative Univariate Analysis for "Age"
mean(clean_data$Age)
median(clean_data$Age)
min(clean_data$Age)
max(clean_data$Age)
range(clean_data$Age)
quantile(clean_data$Age)
# Variance
var(clean_data$Age)

# Standard Deviation
sd(clean_data$Age)

# Shape
skewness(clean_data$Age)
kurtosis(clean_data$Age)


#----------------------------------
#### 2nd variable - Job
summary(clean_data$Job)

#Frequency table
table(clean_data$Job)

# percentage table
table(clean_data$Job)*100/sum(table(clean_data$Job))

# Pie chart
pie(table(clean_data$Job), main = "Job")
dev.copy(png,"plot2.png")
dev.off()
plot(clean_data$Job, xlab = "Job", ylab = "Freqeuncy", main = "Job Description", col = "dark green")


#----------------------------------
#### 3rd variable - Marital
summary(clean_data$Marital)

#Frequency table
table(clean_data$Marital)

# percentage table
table(clean_data$Marital)*100/sum(table(clean_data$Marital))

# Pie chart
pie(table(clean_data$Marital), main = "Marital Status")
dev.copy(png,"plot3.png")
dev.off()
# Bar Chart
plot(clean_data$Marital, xlab = "Num of Marital Status", ylab = "Freqeuncy", main = "Marital Status", col = "green")

#----------------------------------

#### 4th variable - Education
summary(clean_data$Education)

#Frequency table
table(clean_data$Education)

# percentage table
table(clean_data$Education)*100/sum(table(clean_data$Education))

# Pie chart
pie(table(clean_data$Education), main = "Education")
dev.copy(png,"plot4.png")
dev.off()
# Bar Chart
plot(clean_data$Education, xlab = "Num of Education", ylab = "Freqeuncy", main = "Education Graph", col = "dark green")


#----------------------------------

#### 5th variable - Defaulter
summary(clean_data$Defaulter)

#Frequency table
table(clean_data$Defaulter)

# percentage table
table(clean_data$Defaulter)*100/sum(table(clean_data$Defaulter))

# Pie chart
pie(table(clean_data$Defaulter), main = "Defaulter List")
plot(clean_data$Defaulter, xlab = "Defaulter", ylab = "Freqeuncy", main = "Defaulter Rate", col = "green")

#----------------------------------

#### 6th variable - Housing_Loan
summary(clean_data$Housing_Loan)

#Frequency table
table(clean_data$Housing_Loan)

# percentage table
table(clean_data$Housing_Loan)*100/sum(table(clean_data$Housing_Loan))

# Pie chart
pie(table(clean_data$Housing_Loan), main = "House Loan")
plot(clean_data$Housing_Loan, xlab = "Housing Loan", ylab = "Freqeuncy", main = "Housing Loan of Client", col = "Dark green")

#----------------------------------

#### 7th variable - Personal_Loan
summary(clean_data$Personal_Loan)

#Frequency table
table(clean_data$Personal_Loan)

# percentage table
table(clean_data$Personal_Loan)*100/sum(table(clean_data$Personal_Loan))

# Pie chart
pie(table(clean_data$Personal_Loan), main = "Personal Loan")

#----------------------------------

#### 8th variable - Contact
summary(clean_data$Contact)

#Frequency table
table(clean_data$Contact)

# percentage table
table(clean_data$Contact)*100/sum(table(clean_data$Contact))

# Pie chart
pie(table(clean_data$Contact), main = "Contact Mode")
dev.copy(png,"plot5.png")
dev.off()
plot(clean_data$Contact, xlab = "Contact Type", ylab = "Freqeuncy", main = "Mode of Contact", col = "green")

#----------------------------------

#### 9th variable - Marketing done in Month
summary(clean_data$Month)

#Frequency table
table(clean_data$Month)

# percentage table
table(clean_data$Month)*100/sum(table(clean_data$Month))

# Pie chart
pie(table(clean_data$Month), main = "Month")
dev.copy(png,"plot6.png")
dev.off()

#----------------------------------
### 10th variable - Duration since last contacted the same customer
summary(clean_data$Duration)

# Plotting of column - Duration
boxplot(clean_data$Duration, col = "blue", main = "Boxplot of Duration")
dev.copy(png,"plot7.png")
dev.off()
hist(clean_data$Duration, col = "green", xlab = "Duration of Days since last contacted", main = "Histogram of Duration")
plot(density(clean_data$Duration), main="Density Spread")

# Other Values
mean(clean_data$Duration)
median(clean_data$Duration)
min(clean_data$Duration)
max(clean_data$Duration)
range(clean_data$Duration)
quantile(clean_data$Duration)
# Variance
var(clean_data$Duration)

# Standard Deviation
sd(clean_data$Duration)

# Shape
skewness(clean_data$Duration)
kurtosis(clean_data$Duration)

#----------------------------------

### 11th variable - Campaign_Days for the same customer
summary(clean_data$Campaign_Days)

# Plotting of column - Campaign_Days
boxplot(clean_data$Campaign_Days, col = "blue", main = "Boxplot of Campaign Days for Customers")
hist(clean_data$Campaign_Days, col = "green", xlab = "Duration of Campaign Days", main = "Histogram of Campaigning Days")
dev.copy(png,"plot9.png")
dev.off()
plot(density(clean_data$Campaign_Days), main="Density Spread")

# Other Values
mean(clean_data$Campaign_Days)
median(clean_data$Campaign_Days)
min(clean_data$Campaign_Days)
max(clean_data$Campaign_Days)
range(clean_data$Campaign_Days)
quantile(clean_data$Campaign_Days)
# Variance
var(clean_data$Campaign_Days)

# Standard Deviation
sd(clean_data$Campaign_Days)

# Shape
skewness(clean_data$Campaign_Days)
kurtosis(clean_data$Campaign_Days)

#----------------------------------

### 12th variable - Previous_Campaign_Days for the same customer
summary(clean_data$Prev_Camp_Days)

# Plotting of column - Previous_Campaign_Days
boxplot(clean_data$Prev_Camp_Days, col = "blue", main = "Boxplot of Previous Campaign Days for Customers")
hist(clean_data$Prev_Camp_Days, col = "green", xlab = "Duration of Previous Campaign Days", main = "Histogram of Previous Campaigning Days")
plot(density(clean_data$Prev_Camp_Days), main="Density Spread")

# Other Values
mean(clean_data$Prev_Camp_Days)
median(clean_data$Prev_Camp_Days)
min(clean_data$Prev_Camp_Days)
max(clean_data$Prev_Camp_Days)
range(clean_data$Prev_Camp_Days)
quantile(clean_data$Prev_Camp_Days)
# Variance
var(clean_data$Prev_Camp_Days)

# Standard Deviation
sd(clean_data$Prev_Camp_Days)

# Shape
skewness(clean_data$Prev_Camp_Days)
kurtosis(clean_data$Prev_Camp_Days)

#----------------------------------


### 13th variable - Emp_Var_Rate for the same customer
summary(clean_data$Emp_Var_Rate)

# Plotting of column - Previous_Campaign_Days
boxplot(clean_data$Emp_Var_Rate, col = "blue", main = "Boxplot of Employee Variance Rate for Customers")
hist(clean_data$Emp_Var_Rate, col = "green", xlab = "Duration of Employee Variance Rate", main = "Histogram of Employee Variance Rate")
plot(density(clean_data$Emp_Var_Rate), main="Density Spread of Employee Variance Rate")

# Other Values
mean(clean_data$Emp_Var_Rate)
median(clean_data$Emp_Var_Rate)
min(clean_data$Emp_Var_Rate)
max(clean_data$Emp_Var_Rate)
range(clean_data$Emp_Var_Rate)
quantile(clean_data$Emp_Var_Rate)
# Variance
var(clean_data$Emp_Var_Rate)

# Standard Deviation
sd(clean_data$Emp_Var_Rate)

# Shape
skewness(clean_data$Emp_Var_Rate)
kurtosis(clean_data$Emp_Var_Rate)

#----------------------------------

#### 14th variable - Previous Outcome of Marketing campaign
summary(clean_data$Prev_Outcome)

#Frequency table
table(clean_data$Prev_Outcome)

# percentage table
table(clean_data$Prev_Outcome)*100/sum(table(clean_data$Prev_Outcome))

# Pie chart
pie(table(clean_data$Prev_Outcome), main = "Previous Outcome")
dev.copy(png,"plot10.png")
dev.off()

# Bar chart
plot(clean_data$Prev_Outcome, xlab = "Previous Outcome of Marketing Campaign", ylab = "Freqeuncy", main = "Previous  Marketing Campaign", col = "Dark green")


#----------------------------------

#----------------------------------
### 15th variable - number of employees - quarterly indicator
summary(clean_data$No_of_Employees)

# Plotting of column - number of employees - quarterly indicator
boxplot(clean_data$No_of_Employees, col = "blue", main = "Boxplot of No_of_Employees")
hist(clean_data$No_of_Employees, col = "green", xlab = "Number of Employees", main = "Histogram of Number of Employees")
plot(density(clean_data$No_of_Employees), main="Density Spread")

# Other Values
mean(clean_data$No_of_Employees)
median(clean_data$No_of_Employees)
min(clean_data$No_of_Employees)
max(clean_data$No_of_Employees)
range(clean_data$No_of_Employees)
quantile(clean_data$No_of_Employees)
# Variance
var(clean_data$No_of_Employees)

# Standard Deviation
sd(clean_data$No_of_Employees)

# Shape
skewness(clean_data$No_of_Employees)
kurtosis(clean_data$No_of_Employees)

#----------------------------------

#### 16th variable - Bank Term Deposit
summary(clean_data$Term_Deposit)

#Frequency table
table(clean_data$Term_Deposit)

# percentage table
table(clean_data$Term_Deposit)*100/sum(table(clean_data$Term_Deposit))

# Pie chart
pie(table(clean_data$Term_Deposit), main = "Bank Term Deposit")

# Bar chart
plot(clean_data$Term_Deposit, xlab = "Num of Bank Term Deposit Subscribed", ylab = "Freqeuncy", main = "Term Subscribe", col = "green")
#----------------------------------


################################################################


#############################################################
##### Task 4 - Bivariate Analysis #####
#############################################################

str(clean_data)
par(mfrow=c(2,2))

#### Qualitative Bivariate Analysis #####
## For columns "Job" and "Marital"
#Frequency table
table(clean_data$Job, clean_data$Marital)

# percentage table
table(clean_data$Job, clean_data$Marital)*100/sum(table(clean_data$Job, clean_data$Marital))

#bar chart
barplot(table(clean_data$Job, clean_data$Marital))
plot(x=clean_data$Job,y=clean_data$Marital, col = "blue")


## For columns "Job" and "Education"
#Frequency table
table(clean_data$Job, clean_data$Education)

# percentage table
table(clean_data$Job, clean_data$Education)*100/sum(table(clean_data$Job, clean_data$Education))

#bar chart
barplot(table(clean_data$Job, clean_data$Education))


## For columns "Job" and "Defaulter"
#Frequency table
table(clean_data$Job, clean_data$Defaulter)

# percentage table
table(clean_data$Job, clean_data$Defaulter)*100/sum(table(clean_data$Job, clean_data$Defaulter))


## For columns "Education" and "Defaulter"
#Frequency table
table(clean_data$Education, clean_data$Defaulter)

# percentage table
table(clean_data$Education, clean_data$Defaulter)*100/sum(table(clean_data$Education, clean_data$Defaulter))



## For columns "Job" and "Term_Deposit"
#Frequency table
table(clean_data$Job, clean_data$Term_Deposit)

# percentage table
table(clean_data$Job, clean_data$Term_Deposit)*100/sum(table(clean_data$Job, clean_data$Term_Deposit))
#plot(table(clean_data$Job, clean_data$Term_Deposit))
plot(x=clean_data$Job,y=clean_data$Term_Deposit, col = "green", xlab = "Job", ylab = "Subscribe to Term")


## For columns "Marital" and "Term_Deposit"
#Frequency table
table(clean_data$Marital, clean_data$Term_Deposit)

# percentage table
table(clean_data$Marital, clean_data$Term_Deposit)*100/sum(table(clean_data$Marital, clean_data$Term_Deposit))
plot(x=clean_data$Marital,y=clean_data$Term_Deposit, col = "violet", xlab = "Marital Status", ylab = "Subscribe to Term")


## For columns "Marital" and "Housing_Loan"
#Frequency table
table(clean_data$Marital, clean_data$Housing_Loan)

# percentage table
table(clean_data$Marital, clean_data$Housing_Loan)*100/sum(table(clean_data$Housing_Loan, clean_data$Housing_Loan))



## For columns "Education" and "Term_Deposit"
#Frequency table
table(clean_data$Education, clean_data$Term_Deposit)

# percentage table
table(clean_data$Education, clean_data$Term_Deposit)*100/sum(table(clean_data$Education, clean_data$Term_Deposit))

#bar chart
barplot(table(clean_data$Education, clean_data$Term_Deposit))
plot(x=clean_data$Education,y=clean_data$Term_Deposit, col = "light blue", xlab = "Education", ylab = "Subscribe to Term")

---------------------------------------------------------

plot(x=clean_data$Contact,y=clean_data$Term_Deposit, col = "blue", xlab = "Contact Mode", ylab = "Subscribe to Term")
  
  
boxplot(Age~Term_Deposit, data=clean_data, col="red", ylab = "Duration of Days", xlab = "Bank Term Deposit")


#############################################################
#### quantitative Bivariate Analysis
str(clean_data)

## Correlation matrix
cor(clean_data[, c(1,11,12,13,14,16,17,18,19,20)])


corrMatrixData <- clean_data[, c(1,11,12,13,14,16,17,18,19,20)]
round(cor(corrMatrixData),2)

dev.off(); #Resetting par
corrMatrix<-cor(corrMatrixData)
corrplot(corrMatrix, method = "number") # Display the correlation coefficient
------------------------------------------------------------------------------

  
### Bivariate analysis between Age and Campaign_Days
# scatter plots
plot(x=clean_data$Age,y=clean_data$Campaign_Days)

# correlation
cor(clean_data$Age,clean_data$Campaign_Days)

#Covariance
cov(clean_data$Age,clean_data$Campaign_Days)

#linear model
model<-lm(clean_data$Campaign_Days ~ clean_data$Age)

# plot linear line
lines(clean_data$Age, model$fitted.values)


#linear model   and poly
plot(x=clean_data$Age,y=clean_data$Campaign_Days)
model<-lm(clean_data$Campaign_Days~poly(clean_data$Age,1))
lines(clean_data$Age, model$fitted.values)

---------------------------------------------------------------
### Bivariate analysis between "Consumer_Price_Ind" and "Emp_Var_Rate"
# scatter plots
plot(x=clean_data$Consumer_Price_Ind,y=clean_data$Emp_Var_Rate)

# correlation
cor(clean_data$Consumer_Price_Ind,clean_data$Emp_Var_Rate)

#Covariance
cov(clean_data$Consumer_Price_Ind,clean_data$Emp_Var_Rate)

#linear model
model<-lm(clean_data$Emp_Var_Rate ~ clean_data$Consumer_Price_Ind)

# plot lenear line
lines(clean_data$Consumer_Price_Ind, model$fitted.values)


#linear model   and poly
plot(x=clean_data$Consumer_Price_Ind,y=clean_data$Emp_Var_Rate)
model<-lm(clean_data$Emp_Var_Rate~poly(clean_data$Consumer_Price_Ind,2))
lines(clean_data$Consumer_Price_Ind, model$fitted.values)


---------------------------------------------------------------
### Bivariate analysis between "Emp_Var_Rate" and "No_of_Employees"
# scatter plots
plot(x=clean_data$Emp_Var_Rate,y=clean_data$No_of_Employees)

# correlation
cor(clean_data$Emp_Var_Rate,clean_data$No_of_Employees)

#Covariance
cov(clean_data$Emp_Var_Rate,clean_data$No_of_Employees)

#linear model
model<-lm(clean_data$No_of_Employees ~ clean_data$Emp_Var_Rate)

# plot linear line
lines(clean_data$Emp_Var_Rate, model$fitted.values)


#linear model   and poly
plot(x=clean_data$Emp_Var_Rate,y=clean_data$No_of_Employees)
model<-lm(clean_data$No_of_Employees~poly(clean_data$Emp_Var_Rate,2))
lines(clean_data$Emp_Var_Rate, model$fitted.values)





#############################################################
##### Task 5 - Interpretation #####
#############################################################

str(clean_data)
---------------------------------------------------------------
par(mfrow=c(2,3))

boxplot(Age~Term_Deposit, data=clean_data, col="red", main = "Age by Subscription")
boxplot(Campaign_Days~Term_Deposit, data=clean_data, col="blue", main = "Campaign by Subscription")
boxplot( Prev_Camp_Days ~ Term_Deposit, data=clean_data,col="blue", main = " Previous Camp by Subscription")
boxplot( Emp_Var_Rate ~ Term_Deposit, data=clean_data, col="green", main = "Emp.Var.Rate by Subscription")
boxplot( No_of_Employees ~ Term_Deposit, data=clean_data,col="violet", main = "No.of.Emp by Subscription")
boxplot( Duration ~ Term_Deposit, data=clean_data,col="brown", main = "Call Duration by Subscription")

par(mfrow=c(1,1))


#################Plotting:General Visualization#################
par(mfrow=c(2,2),las=2)
boxplot( Duration ~ Term_Deposit, data=clean_data,col="blue",xlab="Term_Deposit", ylab="Duration")
boxplot( Prev_Days_Passed ~ Term_Deposit, data=clean_data,col="red", xlab="Term_Deposit", ylab="Prev_Days_Passed")
plot( clean_data$Housing_Loan, clean_data$Term_Deposit,
      xlab="Housing", ylab="Become Customer?", col=c("red","green"))
plot( clean_data$Contact, clean_data$Term_Deposit,
      xlab="Contact Type", ylab="Become Customer?", col=c("red","green"))

par(mfrow=c(1,1))

#################################################################


par(mfrow=c(2,2))

hist(clean_data$Age, col = "light blue", xlab = "Age", main = "Histogram of Age")
boxplot(Age~Term_Deposit, data=clean_data, col="blue", main = "Age by Subscription")

hist(clean_data$Campaign_Days, col = "violet", main = "Campaign Days")
boxplot(Campaign_Days~Term_Deposit, data=clean_data, col="violet", main = "Campaign by Subscription")

hist(clean_data$Prev_Camp_Days, col = "yellow", main = "Prev Campaign Days")
boxplot( Prev_Camp_Days ~ Term_Deposit, data=clean_data,col="orange", main = " Prev_Camp_Days by Subscription")

hist(clean_data$Emp_Var_Rate, col = "red", main = "Emp Variance Rt")
boxplot( Emp_Var_Rate ~ Term_Deposit, data=clean_data, col="green", main = "Emp.Var.Rate by Subscription")

hist(clean_data$Consumer_Price_Ind, col = "purple", main = "Cons Price Index")
boxplot( Consumer_Price_Ind ~ Term_Deposit, data=clean_data, col="purple", main = "Cons.Prc.Idx by Subscription")

hist(clean_data$No_of_Employees, col = "brown", main = "Num of Employees")
boxplot( No_of_Employees ~ Term_Deposit, data=clean_data,col="brown", main = "No.Employed by Subscription")

hist(clean_data$Duration, col = "grey", main = "Call Duration")
boxplot( Duration ~ Term_Deposit, data=clean_data,col="grey", main = "Call Duration by Subscription")

hist(clean_data$Euri_3_month_Rate, col = "light green", main = "Euribor3_month_Rate")
boxplot( Euri_3_month_Rate ~ Term_Deposit, data=clean_data,col="light green", main = "Euribor3_month by Subscription")




par(mfrow=c(1,1))
