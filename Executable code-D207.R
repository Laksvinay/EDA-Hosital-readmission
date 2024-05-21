#D207 Exploratory Data Analysis

#set the workspace to a specific directory
setwd('C:/Users/laksh/Desktop/WGU-MS Data analytics')

#Import data set to a new dataframe
exdata<-read.csv('C:/Users/laksh/Desktop/WGU-MS Data analytics/MSDA-D207-Exploratory Analysis/Performance assessment D207/medical_clean.csv',header=TRUE)

#install required packages and load libraries
install.packages("dplyr")       #for data manipulation tasks
install.packages("ggplot2")     #for creating and customizing visualizations
install.packages("visdat")      # visualization of missing data
install.packages("naniar")      #for working with missing data

#Load libraries from installed packages
library(dplyr)
library(ggplot2)
library(visdat)
library(plyr) #for label encoding revalue function


#-----------------------------------------------------
#Data Preparation 

#check the datatypes of the variables in data set
glimpse(exdata)

#renaming survey variables in a meaningful way
colnames(exdata)[colnames(exdata)=="Item1"]<-"Timely_admission"
colnames(exdata)[colnames(exdata)=="Item2"]<-"Timely_treatment"
colnames(exdata)[colnames(exdata)=="Item3"]<-"Timely_visits"
colnames(exdata)[colnames(exdata)=="Item4"]<-"Reliability"
colnames(exdata)[colnames(exdata)=="Item5"]<-"Treatment_hours"
colnames(exdata)[colnames(exdata)=="Item6"]<-"Options"
colnames(exdata)[colnames(exdata)=="Item7"]<-"Courteous_staff"
colnames(exdata)[colnames(exdata)=="Item8"]<-"Active_listening"

#Count of all duplicates exists in the data frame
sum(duplicated(exdata))

#count of all missing values in the data frame
colSums((is.na(exdata)))

#checking missing data
vis_miss(exdata)
#------------------------------------------------------
#identify the variables for hypothesis test

#summary statistic of Initial_days
summary(exdata$Initial_days)

#Histogram of Initial_days
ggplot(exdata,aes(x=Initial_days)) + geom_histogram(copalor="white",fill="steelblue")+ ggtitle("Histogram of Initial_days") + theme(plot.title = element_text(hjust = 0.5)) + xlab("Initial_days") + ylab("count") 

#summary of initial length of  stay in hospital
summary(exdata$ReAdmis)

#Assign numeric value using 'label encoding' for categorical variables
 exdata$ReAdmis <-revalue(x=exdata$ReAdmis,replace=c("Yes"=1,"No"=0))

#convert categorical variable class from character to factor
exdata$ReAdmis<-factor(exdata$ReAdmis)

#visualization of ReAdmis variable(readmitted to hospital in one month after discharge)
ggplot(exdata,aes(x=ReAdmis)) + geom_bar(copalor="white",fill="steelblue")+ ggtitle("Readmission status in a month") + theme(plot.title = element_text(hjust = 0.5)) + xlab("Readmission status") + ylab("frequency") 
#----------------------------------------------------------
#Boxplot of Initial_days to confirm the distribution and outliers
boxplot(exdata$Initial_days,main = "Boxplot of Initial_days", ylab = "Initial_days",col="steelblue")

#QQ plot of Initial_days
qqnorm(exdata$Initial_days, main = "QQ plot of Initial_days",col="brown")
qqline(exdata$Initial_days,col="blue")  # Add a reference line

# Calculate mean and standard deviation of Initial_days
mean_value <- mean(exdata$Initial_days)
sd_value <- sd(exdata$Initial_days)

# Calculate z-scores to determine how many standard deviations each point is away from the mean of the distribution
z_scores <- (exdata$Initial_days - mean_value) / sd_value

# Plot normal distribution curve with z-scores
hist(z_scores, breaks = 20, freq = FALSE, main = "Distribution of Initial_days with Z-scores", xlab = "Z-scores", ylab = "Density",col = "steelblue")

#--------------------------------------------------------------------

#Two sample T-test
test_result<-t.test( Initial_days ~ ReAdmis,data=exdata)
#Welch's t-test is used here since the sample sizes of the two groups are unequal.

#Print test_result
print(test_result)

#density plot
ggplot(exdata, aes(x = Initial_days, fill = ReAdmis)) +
  geom_density(alpha = 0.5) +
  labs(title = "Density Plot of Initial Days by Readmission Status",
       x = "Initial Days", y = "Density") +
  scale_fill_discrete(name = "Readmission Status")

#correlation between variables
correlation <- cor(exdata$Initial_days,as.numeric(exdata$ReAdmis))
print(correlation)  #represents the strength and direction of the linear relationship 
#------------------------------------------------------------------------
#C:univariate analysis

# summary statistics for Income and TotalCharge (continuous variables)
summary_income <- summary(exdata$Income)
summary_totalcharge <- summary(exdata$TotalCharge)
Standarddeviation = c(sd(exdata$Income),sd(exdata$TotalCharge))
varianc=c(var(exdata$Initial_days),var(exdata$TotalCharge))

# Combine summary statistics into a data frame
summary_df <- data.frame(
  Variable = c("Income", "TotalCharge"),
  Minimum = c(summary_income[1], summary_totalcharge[1]),
  Median = c(summary_income[3], summary_totalcharge[3]),
  Mean = c(summary_income[4], summary_totalcharge[4]),
  Maximum = c(summary_income[6], summary_totalcharge[6]),
  st.deviation = c(Standarddeviation[1],Standarddeviation[2]),
  variance=c(varianc[1],varianc[2])
  
)

# Print summary statistics as a table
library(knitr)
kable(summary_df, caption = "Summary Statistics for continuous variables Income and TotalCharge")

#-----------------------------------------------------------
#visualization of continuous variables Income and TotalCharge, in one panel
#boxplot
boxplot(exdata$Income,col="steelblue",ylab="Income")
boxplot(exdata$TotalCharge,col="steelblue",ylab="TotalCharge")

#histogram
library(gridExtra)

hist.plotter = function(xx, xlabel, title){
  p = ggplot(data.frame(xx = xx)) + 
    geom_histogram(aes(x= xx),
                   breaks = hist(xx, plot = FALSE)$breaks, 
                   colour="black", fill = "lightblue") +
    ggtitle(title) + 
    theme(plot.title = element_text(hjust = 0.5)) +
    xlab(xlabel) + ylab("Density") +
    theme(axis.text=element_text(size=10),
          axis.title=element_text(size=10),
          plot.title = element_text(size=10))
  p
}

p1 = hist.plotter(exdata$Income, "Initial_days", "Income-positively skewed distribution")
p2 = hist.plotter(exdata$TotalCharge, "TotalCharge", "TotalCharge-Bimodal distribution")
# Combine plots
grid.arrange(p1, p2, nrow = 1)
#--------------------------------------------------------------------
#Frequency table of categorical variable Services 
addmargins(table(exdata$Services)) 
summary(exdata$Services)

#Frequency table ofcategorical variable Timely_treatment
addmargins(table(exdata$Timely_treatment))  
summary(exdata$Timely_treatment)

#visualization of categorical variables in one panel
hist.plotter = function(data, xlabel, title){
  b = ggplot(data) + 
    geom_bar(aes(x = xx, fill = xx), alpha = 0.5) +
    ggtitle(title) + 
    theme(plot.title = element_text(hjust = 0.5)) +
    xlab(xlabel)  +
    theme(axis.text=element_text(size=10),
          axis.title=element_text(size=10),
          plot.title = element_text(size=10))
  b
}


b1 = hist.plotter(data.frame(xx = exdata$Services), "Services", "barplot of Services")
b2 = hist.plotter(data.frame(xx = exdata$Timely_treatment), "Timely_treatment", "barplot of Timely_treatment")

# Combine categorical plots
grid.arrange(b1,b2, nrow =1)

#----------------------------------------------------------------------
#Bivariate analysis

#continuous vs. continuous
summary_Age <- summary(exdata$Age)
summary_vitaminD <- summary(exdata$VitD_levels)
standarddev <- c(sd(exdata$Age),sd(exdata$VitD_levels)) # results are stored in a vector
varnce <- c(var(exdata$Age),var(exdata$VitD_levels))

#creating dataframe using vectors or values extracted 
summary_df <- data.frame(
  Variable = c("Age", "VitD_levels"),
  Minimum = c(summary_Age[1], summary_vitaminD[1]),
  Median = c(summary_Age[3], summary_vitaminD[3]),
  Mean = c(summary_Age[4], summary_vitaminD[4]),
  Maximum = c(summary_Age[6], summary_vitaminD[6]),
  Standard.deviation=c(standarddev[1],standarddev[2]),
  variance=c(varnce[1],varnce[2])
  
  
)

# Print summary statistics as a table
kable(summary_df, caption = "Summary Statistics for continuous variables Age and VitD_levels")


print(paste("Correlation coefficient between Age and VitD_levels is:",cor(exdata$Age,exdata$VitD_levels)))
#correlation coefficient is close to zero (absolute value), a very weak positive linear relationship between the two variables.

#scatter with line of best fit
ggplot(data = exdata, aes(x = Age, y = VitD_levels)) +
  geom_point(col="brown") +  # Add points for each observation
  geom_smooth(method = "lm", se = FALSE) +  # Add a line of best fit using linear regression
  labs(x = "Age", y = "VitD Levels") +  # Label the axes
  ggtitle("Scatter Plot of Age vs. VitD Levels with Line of Best Fit")  # Add a title
#------------------------------------------------------

#categorical vs. categorical
print(table(exdata$Gender))
summary(exdata$Gender)
print(table(exdata$ReAdmis))
summary(exdata$ReAdmis)


#cross tab/contingency table for catgeorical variables Gender and ReAdmis
contingency_tab <- xtabs(~ Gender + ReAdmis, data = exdata)
addmargins(contingency_tab)
summary(contingency_tab)

#--------------------------------------------------------------

#visualization of bivariate statistics of  categorical variables 
ggplot(exdata, aes(x = Gender, fill = ReAdmis)) +
  geom_bar(position = "fill") +
  labs(title = "Proportion of ReAdmis Cases by Gender",
       x = "Gender", y = "Proportion",
       fill = "ReAdmis") +
  scale_fill_manual(values = c("steelblue", "gold"), labels = c("No", "Yes")) +  # Custom fill colors and labels
  theme_minimal()

#second visualization
# Create a data frame with counts of each combination of Gender and ReAdmis
counts <- table(exdata$Gender, exdata$ReAdmis) #creates a contingency table that shows the counts
#Frequency table for categorical variables
print(counts)
counts <- as.data.frame(counts) #resulting table converted into a dataframe
names(counts) <- c("Gender", "ReAdmis", "Frequency")    #column names of the dataframe 
# barchart of 2 categorical variables
ggplot(counts, aes(x = Gender, y = Frequency, fill = ReAdmis)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Bar Chart of ReAdmis by Gender", x = "Gender", y = "Frequency") 
theme_minimal()

#---------------------------------------------------------------------------------------