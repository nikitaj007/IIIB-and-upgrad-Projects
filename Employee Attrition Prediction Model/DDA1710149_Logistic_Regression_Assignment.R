#####################################################################################
# Clean the Environment
#####################################################################################
rm(list=ls())
setwd("C:/Users/212431278/Desktop/IIIT B/Logistic regression")

#####################################################################################
# loading required Libraries
#####################################################################################

library(MASS)
library(car)
library(corrplot)
library(e1071)
library(caret)
library(ROCR)
library(dplyr)
library(ggplot2)
library(cowplot)

#####################################################################################
# Creating required functions
#####################################################################################

getmode <- function(col) {
  uniqv <- unique(col)
  uniqv[which.max(tabulate(match(col, uniqv)))]
}


MyMerge <- function(x, y){ 
  df <- merge(x, y, by= "EmployeeID", all.x= TRUE, all.y= TRUE) 
  return(df)
  } 

perform_fn <- function(cutoff) 
{
  test_actual<- factor(ifelse(test$Attrition==1,"Yes","No"))
  predicted_Attrition <- factor(ifelse(test_pred >= cutoff, "Yes", "No"))
  conf <- confusionMatrix(predicted_Attrition, test_actual, positive = "Yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}


lift <- function(labels , predicted_prob,groups=10) {
  
  if(is.factor(labels)) labels  <- as.integer(as.character(labels ))
  if(is.factor(predicted_prob)) predicted_prob <- as.integer(as.character(predicted_prob))
  helper = data.frame(cbind(labels , predicted_prob))
  helper[,"bucket"] = ntile(-helper[,"predicted_prob"], groups)
  gaintable = helper %>% group_by(bucket)  %>%
    summarise_at(vars(labels ), funs(total = n(),
                                     totalresp=sum(., na.rm = TRUE))) %>%
    
    mutate(Cumresp = cumsum(totalresp),
           Gain=Cumresp/sum(totalresp)*100,
           Cumlift=Gain/(bucket*(100/groups))) 
  return(gaintable)
}


#####################################################################################
# loading data
#####################################################################################

emp_sur<-read.csv("employee_survey_data.csv",stringsAsFactors = F)
gen_data<-read.csv("general_data.csv",stringsAsFactors = F)
Man_sur<-read.csv("manager_survey_data.csv",stringsAsFactors = F)
in_time<-read.csv("in_time.csv",stringsAsFactors = F)
out_time<-read.csv("out_time.csv",stringsAsFactors = F)

#####################################################################################
# Understanding and cleaning the data
#####################################################################################

#first look at the summary of each dataset
#check for duplicate values and NA values
#structure of the data

summary(emp_sur)
str(emp_sur) #4410 X 4
which(duplicated(emp_sur)==TRUE)
which(is.na(emp_sur)==TRUE)
as.data.frame(sapply(emp_sur,function(x) sum(is.na(x))))
#EnvironmentSatisfaction(25) ,JobSatisfaction(20) and WorkLifeBalance(38) has NA values
#No duplicates found


summary(gen_data)
str(gen_data) # 4410 X 24 
which(duplicated(gen_data)==TRUE) # no duplicate values
as.data.frame(sapply(gen_data,function(x) sum(is.na(x))))
#NumCompaniesWorked(19) , TotalWorkingYears(9) has NA values

summary(Man_sur)
str(Man_sur) #4410 X 3
which(duplicated(Man_sur)==TRUE) # no duplicates
which(is.na(Man_sur)==TRUE) # there are no NA values


summary(in_time)
str(in_time)
which(duplicated(in_time)==TRUE) # no duplicate values
which(is.na(in_time)==TRUE) # NA values are nothing but employee is on leave / holiday
as.data.frame(sapply(in_time,function(x) sum(is.na(x))))
## Rename X column to EmployeeID
colnames(in_time)[1]<- "EmployeeID"


summary(out_time)
str(out_time) 
which(duplicated(out_time)==TRUE) # no duplicate values
which(is.na(out_time)==TRUE) # NA values are nothing but employee is on leave / holiday
as.data.frame(sapply(out_time,function(x) sum(is.na(x))))
## Rename X column to EmployeeID
colnames(out_time)[1]<- "EmployeeID"

num_of_holidays<-length(which(colSums(is.na(in_time))==4410))
num_of_leaves<-rowSums(is.na(in_time))-num_of_holidays

time_diff<-sapply(2:262, function(i) difftime(time1 = as.POSIXct(out_time[,i],origin = "1960-01-01"), time2 = as.POSIXct(in_time[,i],origin = "1960-01-01"), units = "hours"))
avg_time<-.rowMeans(time_diff,4410,261,na.rm = T)

time_df<-as.data.frame(cbind(EmployeeID = in_time[,1],avg_time,num_of_leaves))


#####################################################################################
#Handling NA values- replacing the categorical values by mode
#####################################################################################


emp_sur$EnvironmentSatisfaction[which(is.na(emp_sur$EnvironmentSatisfaction)==T)] <- getmode(emp_sur$EnvironmentSatisfaction)
emp_sur$JobSatisfaction[which(is.na(emp_sur$JobSatisfaction)==T)] <- getmode(emp_sur$JobSatisfaction)
emp_sur$WorkLifeBalance[which(is.na(emp_sur$WorkLifeBalance)==T)] <- getmode(emp_sur$WorkLifeBalance)



#replace the 0 with 2 for NumCompaniesWorked as the difference between TotalWorkingYears and YearsAtCompany is 1
gen_data$TotalWorkingYears[which(gen_data$NumCompaniesWorked==0)] - gen_data$YearsAtCompany[which(gen_data$NumCompaniesWorked==0)]
gen_data$NumCompaniesWorked[which(gen_data$NumCompaniesWorked==0)]<-2

avg_years_in_one_company<-round(sum(gen_data$TotalWorkingYears,na.rm = T)/sum(gen_data$NumCompaniesWorked,na.rm = T))
#avg_years_in_one_company is 4

gen_data$TotalWorkingYears<-
  ifelse(is.na(gen_data$TotalWorkingYears),
         (gen_data$YearsAtCompany+(avg_years_in_one_company*gen_data$NumCompaniesWorked)),
         gen_data$TotalWorkingYears)

gen_data$NumCompaniesWorked<-
  ifelse(is.na(gen_data$NumCompaniesWorked),
         round(((gen_data$TotalWorkingYears- gen_data$YearsAtCompany)/avg_years_in_one_company)+1),gen_data$NumCompaniesWorked)

#####################################################################################
# merging
#####################################################################################

# Collate the data together in one single file
setdiff(gen_data$EmployeeID,Man_sur$EmployeeID)    #Check identical customer ID across all the data set
setdiff(gen_data$EmployeeID,emp_sur$EmployeeID) #Check identical customer ID across all the data set
setdiff(gen_data$EmployeeID,time_df$EmployeeID)  #Check identical customer ID across all the data set

HR_data <- Reduce(MyMerge, list(gen_data, Man_sur, emp_sur, time_df))

#####################################################################################
# Data Preparation & Exploratory Data Analysis 
#####################################################################################

# Understanding the structure of the collated file
str(HR_data) #4410 obs. of  31 variables;

# Barcharts for categorical features with stacked telecom information
bar_theme<- theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), 
                   legend.position="none")
geom_text_style<-geom_text(stat='count',aes(label=..count..),position = position_stack(vjust = 0.5,),size=3)

# plots

plot_grid(ggplot(HR_data,aes(x=Age,fill=Attrition))+geom_bar()+bar_theme+geom_text_style,
          ggplot(HR_data,aes(x=BusinessTravel,fill=Attrition))+geom_bar()+bar_theme+geom_text_style,
          ggplot(HR_data,aes(x=Department,fill=Attrition))+geom_bar()+bar_theme+geom_text_style,
          ggplot(HR_data,aes(x=JobLevel,fill=Attrition))+geom_bar()+bar_theme+geom_text_style)

plot_grid(ggplot(HR_data,aes(x=Education,fill=Attrition))+geom_bar()+bar_theme+geom_text_style,
          ggplot(HR_data,aes(x=EducationField,fill=Attrition))+geom_bar()+bar_theme+geom_text_style,
          ggplot(HR_data,aes(x=Gender,fill=Attrition))+geom_bar()+bar_theme+geom_text_style,
          ggplot(HR_data,aes(x=MaritalStatus,fill=Attrition))+geom_bar()+bar_theme+geom_text_style)

plot_grid(ggplot(HR_data,aes(x=JobRole,fill=Attrition))+geom_bar()+bar_theme+geom_text_style,
          ggplot(HR_data,aes(x=StockOptionLevel,fill=Attrition))+geom_bar()+bar_theme+geom_text_style,
          ggplot(HR_data,aes(x=TotalWorkingYears,fill=Attrition))+geom_bar()+bar_theme+geom_text_style,
          ggplot(HR_data,aes(x=TrainingTimesLastYear,fill=Attrition))+geom_bar()+bar_theme+geom_text_style)

plot_grid(ggplot(HR_data,aes(x=YearsAtCompany,fill=Attrition))+geom_bar()+bar_theme+geom_text_style,
          ggplot(HR_data,aes(x=YearsSinceLastPromotion,fill=Attrition))+geom_bar()+bar_theme+geom_text_style,
          ggplot(HR_data,aes(x=YearsWithCurrManager,fill=Attrition))+geom_bar()+bar_theme+geom_text_style,
          ggplot(HR_data,aes(x=num_of_leaves,fill=Attrition))+geom_bar()+bar_theme+geom_text_style)

plot_grid(ggplot(HR_data,aes(x=EnvironmentSatisfaction,fill=Attrition))+geom_bar()+bar_theme+geom_text_style,
          ggplot(HR_data,aes(x=JobSatisfaction,fill=Attrition))+geom_bar()+bar_theme+geom_text_style,
          ggplot(HR_data,aes(x=WorkLifeBalance,fill=Attrition))+geom_bar()+bar_theme+geom_text_style,
          ggplot(HR_data,aes(x=JobInvolvement,fill=Attrition))+geom_bar()+bar_theme+geom_text_style)

plot_grid(ggplot(HR_data,aes(x=PerformanceRating,fill=Attrition))+geom_bar()+bar_theme+geom_text_style,
          ggplot(HR_data,aes(x=DistanceFromHome,fill=Attrition))+geom_bar()+bar_theme+geom_text_style,
          ggplot(HR_data,aes(x=NumCompaniesWorked,fill=Attrition))+geom_bar()+bar_theme+geom_text_style)

ggplot(HR_data,aes(x=NumCompaniesWorked,fill=Attrition))+geom_bar(position = "Fill")+bar_theme
ggplot(HR_data,aes(x=YearsSinceLastPromotion,fill=Attrition))+geom_bar(position = "Fill")+bar_theme
ggplot(HR_data,aes(x=Age,fill=Attrition))+geom_bar(position = "Fill")+bar_theme
ggplot(HR_data,aes(x=TotalWorkingYears,fill=Attrition))+geom_bar(position = "Fill")+bar_theme
ggplot(HR_data,aes(x=YearsWithCurrManager,fill=Attrition))+geom_bar(position = "Fill")+bar_theme

# Histogram and Boxplots for numeric variables 
boxplot(HR_data$Age) #No outliers
boxplot(HR_data$Age~HR_data$Attrition)
boxplot(HR_data$MonthlyIncome) # it has outliers
boxplot(HR_data$MonthlyIncome~HR_data$Attrition)
quantile(HR_data$MonthlyIncome,seq(0,1,0.01))
HR_data$MonthlyIncome[which(HR_data$MonthlyIncome>quantile(HR_data$MonthlyIncome,0.75)+1.5*(IQR(HR_data$MonthlyIncome)))]<-quantile(HR_data$MonthlyIncome,0.75)+1.5*(IQR(HR_data$MonthlyIncome))


boxplot(HR_data$YearsSinceLastPromotion~HR_data$Attrition)
boxplot(HR_data$YearsSinceLastPromotion)
quantile(HR_data$YearsSinceLastPromotion)
HR_data$YearsSinceLastPromotion[which(HR_data$YearsSinceLastPromotion>quantile(HR_data$YearsSinceLastPromotion,0.75)+1.5*(IQR(HR_data$YearsSinceLastPromotion)))]<-quantile(HR_data$YearsSinceLastPromotion,0.75)+1.5*(IQR(HR_data$YearsSinceLastPromotion))

boxplot(HR_data$YearsWithCurrManager)

boxplot(HR_data$DistanceFromHome) #No outliers
boxplot(HR_data$PercentSalaryHike) #No outliers
boxplot(HR_data$PercentSalaryHike~HR_data$Attrition)
boxplot(HR_data$TotalWorkingYears) #it has outliers
boxplot(HR_data$TotalWorkingYears~HR_data$Attrition)
quantile(HR_data$TotalWorkingYears)
HR_data$TotalWorkingYears[which(HR_data$TotalWorkingYears>quantile(HR_data$TotalWorkingYears,0.75)+1.5*(IQR(HR_data$TotalWorkingYears)))]<-quantile(HR_data$TotalWorkingYears,0.75)+1.5*(IQR(HR_data$TotalWorkingYears))

boxplot(HR_data$NumCompaniesWorked) #No outliers
boxplot(HR_data$YearsAtCompany) # it has outliers
quantile(HR_data$YearsAtCompany)
boxplot(HR_data$YearsAtCompany~HR_data$Attrition)
HR_data$YearsAtCompany[which(HR_data$YearsAtCompany>quantile(HR_data$YearsAtCompany,0.75)+1.5*(IQR(HR_data$YearsAtCompany)))]<-quantile(HR_data$YearsAtCompany,0.75)+1.5*(IQR(HR_data$YearsAtCompany))


# one value columns can be removed
which(as.data.frame(sapply(HR_data,function(x) length(unique(x))))[1]==1)
HR_data <- HR_data[,-c(9,16,18)]
str(HR_data)

#####################################################################################
# Correlation between continous variables
#####################################################################################
# Let's see the correlation values of the numerical variables 
continous_var <- names(HR_data)[c(2,6,13,14,15,17,18,19,20,21,28)]
continous_var
continous_data <- HR_data[,(colnames(HR_data) %in% continous_var)]
corr <- cor(continous_data)

# Plot the correlation matrix to understand the correlation between variables.
corrplot(corr, method="number")

#Age and TotalworkingYears are highly correlated
#Yearatcompany and yearwithcurrentmanager are highly correlated

#####################################################################################
# derived metrics and segmented variables
#####################################################################################

HR_data$avg_time<- ifelse(HR_data$avg_time<8,"below_8_Hours","8+Hours")


#####################################################################################
# creation of dummy variables and Feature standardisation
#####################################################################################

# Normalising continuous features 
HR_data$MonthlyIncome <- scale(HR_data$MonthlyIncome)
HR_data$num_of_leaves <- scale(HR_data$num_of_leaves)
HR_data$PercentSalaryHike <- scale(HR_data$PercentSalaryHike)
HR_data$Age <- scale(HR_data$Age)
HR_data$DistanceFromHome <- scale(HR_data$DistanceFromHome)
HR_data$NumCompaniesWorked <- scale(HR_data$NumCompaniesWorked)
HR_data$TotalWorkingYears <- scale(HR_data$TotalWorkingYears)
HR_data$TrainingTimesLastYear <- scale(HR_data$TrainingTimesLastYear)
HR_data$YearsAtCompany <- scale(HR_data$YearsAtCompany)
HR_data$YearsSinceLastPromotion <- scale(HR_data$YearsSinceLastPromotion)
HR_data$YearsWithCurrManager <- scale(HR_data$YearsWithCurrManager)



# converting target variable telecom from No/Yes character to factorwith levels 0/1 
HR_data$Attrition<- ifelse(HR_data$Attrition=="Yes",1,0)

Attrition_rate <- sum(HR_data$Attrition)/nrow(HR_data)
Attrition_rate #16%

# creating a dataframe of categorical features
HR_data_chr<- HR_data[,-c(1,2,3,6,13,14,15,17,18,19,20,21,28)]

# converting categorical attributes to factor
HR_data_factor<- data.frame(sapply(HR_data_chr, function(x) factor(x)))
str(HR_data_factor)
# creating dummy variables for factor attributes
dummies<- data.frame(sapply(HR_data_factor, 
                            function(x) data.frame(model.matrix(~x-1,data =HR_data_factor))[,-1]))



HR_data_final<- cbind(HR_data[,c(2,3,6,13,14,15,17,18,19,20,21,28)],dummies) 
str(HR_data_final)

#####################################################################################
# creating training and test data  
#####################################################################################
# separate training and testing data
set.seed(100)
trainindices= sample(1:nrow(HR_data_final), 0.7*nrow(HR_data_final))
train = HR_data_final[trainindices,]
test = HR_data_final[-trainindices,]

#####################################################################################
#Model Building
#####################################################################################

# Logistic Regression: 

#Initial model - Build model 1 containing all variables
model_1 <- glm(Attrition~.,data=train,family = "binomial")
summary(model_1)

# used STEPAIC to find the best model
model_2 <- stepAIC(model_1,direction = "both")
summary(model_2)

# Removing multicollinearity through VIF check
sort(vif(model_2))

#captured the STEPAIC output and built model2

#Excluding MaritalStatus.xMarried due to low significance and high VIF
model_3<-glm(formula = Attrition ~ Age + MonthlyIncome + NumCompaniesWorked + 
               TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
               YearsWithCurrManager + BusinessTravel.xTravel_Frequently + 
               BusinessTravel.xTravel_Rarely + Education.x5 + EducationField.xLife.Sciences + 
               EducationField.xMarketing + EducationField.xMedical + EducationField.xOther + 
               EducationField.xTechnical.Degree + JobLevel.x5 + JobRole.xManufacturing.Director + 
               JobRole.xResearch.Director + JobRole.xSales.Executive + 
               MaritalStatus.xSingle + StockOptionLevel.x1 + JobInvolvement.x2 + 
               JobInvolvement.x3 + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
               EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
               JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
               WorkLifeBalance.x4 + avg_time, family = "binomial", data = train)
summary(model_3)
sort(vif(model_3))

#Excluding MonthlyIncome due to low significance 
model_4<-glm(formula = Attrition ~ Age + NumCompaniesWorked + 
               TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
               YearsWithCurrManager + BusinessTravel.xTravel_Frequently + 
               BusinessTravel.xTravel_Rarely + Education.x5 + EducationField.xLife.Sciences + 
               EducationField.xMarketing + EducationField.xMedical + EducationField.xOther + 
               EducationField.xTechnical.Degree + JobLevel.x5 + JobRole.xManufacturing.Director + 
               JobRole.xResearch.Director + JobRole.xSales.Executive + MaritalStatus.xSingle + 
               StockOptionLevel.x1 + JobInvolvement.x2 + JobInvolvement.x3 + 
               EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
               EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
               JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
               WorkLifeBalance.x4 + avg_time, family = "binomial", data = train)
summary(model_4)
sort(vif(model_4))

#Excluding JobInvolvement.x2 due to low significance
model_5<-glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
               TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
               BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
               Education.x5 + EducationField.xLife.Sciences + EducationField.xMarketing + 
               EducationField.xMedical + EducationField.xOther + EducationField.xTechnical.Degree + 
               JobLevel.x5 + JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
               JobRole.xSales.Executive + MaritalStatus.xSingle + StockOptionLevel.x1 + 
               JobInvolvement.x3 + EnvironmentSatisfaction.x2 + 
               EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
               JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
               WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 + 
               avg_time, family = "binomial", data = train)

summary(model_5)
sort(vif(model_5))

#Excluding JobLevel.x5 due to low significance
model_6<-glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
               TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
               BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
               Education.x5 + EducationField.xLife.Sciences + EducationField.xMarketing + 
               EducationField.xMedical + EducationField.xOther + EducationField.xTechnical.Degree + 
               JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
               JobRole.xSales.Executive + MaritalStatus.xSingle + StockOptionLevel.x1 + 
               JobInvolvement.x3 + EnvironmentSatisfaction.x2 + 
               EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
               JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
               WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 + 
               avg_time, family = "binomial", data = train)
summary(model_6)
sort(vif(model_6))

#Excluding JobRole.xResearch.Director due to low significance
model_7<-glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
               TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
               BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
               Education.x5 + EducationField.xLife.Sciences + EducationField.xMarketing + 
               EducationField.xMedical + EducationField.xOther + EducationField.xTechnical.Degree + 
               JobRole.xManufacturing.Director +
               JobRole.xSales.Executive + MaritalStatus.xSingle + StockOptionLevel.x1 + 
               JobInvolvement.x3 + EnvironmentSatisfaction.x2 + 
               EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
               JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
               WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 + 
               avg_time, family = "binomial", data = train)
summary(model_7)
sort(vif(model_7))

#Excluding StockOptionLevel.x1 due to low significance
model_7<-glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
               TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
               BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
               Education.x5 + EducationField.xLife.Sciences + EducationField.xMarketing + 
               EducationField.xMedical + EducationField.xOther + EducationField.xTechnical.Degree + 
               JobRole.xManufacturing.Director +
               JobRole.xSales.Executive + MaritalStatus.xSingle + 
               JobInvolvement.x3 + EnvironmentSatisfaction.x2 + 
               EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
               JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
               WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 + 
               avg_time, family = "binomial", data = train)
summary(model_7)
sort(vif(model_7))

#Excluding Education.x5 due to low significance
model_8<-glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
               TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
               BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
               EducationField.xLife.Sciences + EducationField.xMarketing + 
               EducationField.xMedical + EducationField.xOther + EducationField.xTechnical.Degree + 
               JobRole.xManufacturing.Director +
               JobRole.xSales.Executive + MaritalStatus.xSingle + 
               JobInvolvement.x3 + EnvironmentSatisfaction.x2 + 
               EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
               JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
               WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 + 
               avg_time, family = "binomial", data = train)
summary(model_8)
sort(vif(model_8))

#Excluding JobInvolvement.x3 due to low significance
model_9<-glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
               TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
               BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
               EducationField.xLife.Sciences + EducationField.xMarketing + 
               EducationField.xMedical + EducationField.xOther + EducationField.xTechnical.Degree + 
               JobRole.xManufacturing.Director +
               JobRole.xSales.Executive + MaritalStatus.xSingle + 
               EnvironmentSatisfaction.x2 + 
               EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
               JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
               WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 + 
               avg_time, family = "binomial", data = train)
summary(model_9)
sort(vif(model_9))

#Excluding JobRole.xSales.Executive due to low significance
model_10<-glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                EducationField.xLife.Sciences + EducationField.xMarketing + 
                EducationField.xMedical + EducationField.xOther + EducationField.xTechnical.Degree + 
                JobRole.xManufacturing.Director +
                MaritalStatus.xSingle + 
                EnvironmentSatisfaction.x2 + 
                EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 + 
                avg_time, family = "binomial", data = train)
summary(model_10)
sort(vif(model_10))

#Excluding JobSatisfaction.x2 due to low significance
model_11<-glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                EducationField.xLife.Sciences + EducationField.xMarketing + 
                EducationField.xMedical + EducationField.xOther + EducationField.xTechnical.Degree + 
                JobRole.xManufacturing.Director + MaritalStatus.xSingle + 
                EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                EnvironmentSatisfaction.x4 + JobSatisfaction.x3 + 
                JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                WorkLifeBalance.x4 + avg_time, family = "binomial", data = train)
summary(model_11)
sort(vif(model_11))

#Excluding JobSatisfaction.x3 due to low significance
model_12<-glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                EducationField.xLife.Sciences + EducationField.xMarketing + 
                EducationField.xMedical + EducationField.xOther + EducationField.xTechnical.Degree + 
                JobRole.xManufacturing.Director + MaritalStatus.xSingle + 
                EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                EnvironmentSatisfaction.x4 + JobSatisfaction.x4 + 
                WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 + 
                avg_time, family = "binomial", data = train)
summary(model_12)
sort(vif(model_12))

#Excluding TrainingTimesLastYear due to low significance
model_13<-glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                YearsSinceLastPromotion + YearsWithCurrManager + 
                BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                EducationField.xLife.Sciences + EducationField.xMarketing + 
                EducationField.xMedical + EducationField.xOther + EducationField.xTechnical.Degree + 
                JobRole.xManufacturing.Director + MaritalStatus.xSingle + 
                EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                EnvironmentSatisfaction.x4 + JobSatisfaction.x4 + WorkLifeBalance.x2 + 
                WorkLifeBalance.x3 + WorkLifeBalance.x4 + avg_time, family = "binomial", 
              data = train)
summary(model_13)
sort(vif(model_13))

#Excluding BusinessTravel.xTravel_Rarely due to low significance
model_14 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                  YearsSinceLastPromotion + YearsWithCurrManager + BusinessTravel.xTravel_Frequently + 
                  EducationField.xLife.Sciences + 
                  EducationField.xMarketing + EducationField.xMedical + EducationField.xOther + 
                  EducationField.xTechnical.Degree + JobRole.xManufacturing.Director + 
                  MaritalStatus.xSingle + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                  EnvironmentSatisfaction.x4 + JobSatisfaction.x4 + WorkLifeBalance.x2 + 
                  WorkLifeBalance.x3 + WorkLifeBalance.x4 + avg_time, family = "binomial", 
                data = train)
summary(model_14)
sort(vif(model_14))
##we take model_13 as our final model as all the variables are highly significant and the VIFs are also not very high and respectable

#####################################################################################
#validation using test data
#####################################################################################


test_pred<-predict(model_13, type = "response",newdata = test[,-2])

summary(test_pred)
test$prob <- test_pred

test_actual<- factor(ifelse(test$Attrition==1,"Yes","No"))

s = seq(.01,.80,length=100)
OUT = matrix(0,100,3)
for(i in 1:100){  
    OUT[i,] = perform_fn(s[i])
} 


#Plot to choose best cutoff
plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0.6,.80,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))


best_cutoff <- s[which(abs(OUT[,1]-OUT[,2])==min(abs(OUT[,1]-OUT[,2])))]

# best_cutoff is 0.185
test_pred_Attrition <- ifelse(test_pred >= best_cutoff, "Yes", "No")
confusionmartix_final<-confusionMatrix(test_pred_Attrition, test_actual, positive = "Yes")

#####################################################################################
#Metrics - Accuracy,Sensitivity,Specificity,KS -statistic
#Lift & Gain Chart 
#####################################################################################

#confusion matrix output
confusionmartix_final
#Accuracy : 0.7498
#Sensitivity : 0.7488         
#Specificity : 0.75

##################################################################################################
### KS -statistic - Test Data ######

test_pred_Attrition <- ifelse(test_pred_Attrition=="Yes",1,0)
test_actual <- ifelse(test_actual=="Yes",1,0)


pred_object_test<- prediction(test_pred_Attrition, test_actual)
performance_measures_test<- performance(pred_object_test, "tpr", "fpr")

ks_table_test <- attr(performance_measures_test, "y.values")[[1]] - 
  (attr(performance_measures_test, "x.values")[[1]])
max(ks_table_test)
#0.498

####################################################################
# Lift & Gain Chart

Churn_decile <- lift(test_actual,test_pred , groups = 10)
Churn_decile

#### ROC Curve #######################

plot(performance_measures_test,
     main="Attrition - ROC Curves",
     xlab="Specificity: False Positive Rate",
     ylab="Sensitivity: True Positive Rate",
     col="blue")
abline(0,1,col="grey")

#### Gain Chart ######################

plot(cbind(seq(0,10,1),c(0,Churn_decile$Gain)),
     xlim=c(0,10),
     ylim=c(0,100),
     main="Gain Chart",
     xlab="Decile",
     ylab="Gain(%)",
     col="blue",
     type="l")
abline(a=0, b=10,col="grey")

# calculating AUC
auc <- performance(pred_object_test,"auc")
# now converting S4 class to vector
auc <- unlist(slot(auc, "y.values")) #0.7493961

#######Lift Chart ####################

plot(Churn_decile$Cumlift,
     main="Attrition - Lift Chart",
     xlab="Decile",
     ylab="Lift",
     col="blue",
     type="l")
abline(1,0,col="grey")

#####################################################################################
#Data preparation :
#####################################################################################
#1. understood the data by looking at the summary and structure
#2.Checked NA and duplicate values
#3. removed outlier in the data and replaced with the 1.5 times of inter quantile range values
#4.Created derived metrics
#5.merged all the datasets
#6.Created dummy varaibles for all the categorical values and scaled the continuous variables
#####################################################################################
#Data modeling :
#####################################################################################
#1.Devided the final data into training and testing data base on random 70:30 sampling.
#2.model_1 created using all the varibales in the model. 
#3.using STEPAIC function, created the model_2 with the out of SETPAIC which gives best variables.
#4.removed one varaible at a time based the VIF and P values by checking adj AIC value after each 
#  model buliding. after 13 iterations,we are left with few varaibles which are highly significant 
#5.validated against test data and the metrics details below
#Accuracy : 0.7498
#Sensitivity : 0.7488         
#Specificity : 0.7500
#Ks_Statistic : 04987
#AUC : 74.93

#####################################################################################
#Model understanding and insights :
##################################################################################### 
'The final Model suggests us that the factors for attrition can be grouped in 2 segments, one who presence will indicate high likeliness to attrition (Positive Co-efficient Value) and the other whose presence signifies less likeliness to attrition  (Negative Co-efficient value). Thus the following table can be obtained.

POSITIVE Co-efficient Value (Strong Indicator of Attrition)
NumCompaniesWorked, YearsSinceLastPromotion, BusinessTravel.xTravel_Frequently, MaritalStatus.xSingle

NEGATIVE Co-efficient Value (Weak Indicator of Attrition)
Age, TotalWorkingYears, YearsWithCurrManager, EducationField.xLife.Sciences, EducationField.xMarketing, EducationField.xMedical, EducationField.xOther, EducationField.xTechnical.Degree, JobRole.xManufacturing.Director, EnvironmentSatisfaction.x2, EnvironmentSatisfaction.x3, EnvironmentSatisfaction.x4, JobSatisfaction.x4, WorkLifeBalance.x2, WorkLifeBalance.x3, WorkLifeBalance.x4, avg_time

Now, evaluating the negative factors, we understand that many of them are categorical variables, and if we look at the supplementary levels of these variables we can fairly caterogise them as a postive factor. i.e.
EnvironmentSatisfaction.x2, EnvironmentSatisfaction.x3, EnvironmentSatisfaction.x4 are under NEGATIVE indicators, hence we fairly can say that EnvironmentSatisfaction.x1 is a POSTIVE indicator of attrition.
WorkLifeBalance.x2, WorkLifeBalance.x3, WorkLifeBalance.x4 are under NEGATIVE indicators, hence we fairly can say that WorkLifeBalance.x1 is a POSTIVE indicator of attrition.
JobSatisfaction.x4 is under NEGATIVE indicators, hence we fairly can say that JobSatisfaction.x1, JobSatisfaction.x2, JobSatisfaction.x3 are POSTIVE indicators of attrition.
EducationField.xLife.Sciences, EducationField.xMarketing, EducationField.xMedical, EducationField.xOther, EducationField.xTechnical.Degree are under NEGATIVE indicators, hence we fairly can say that EducationField.xHuman.Resources is a POSTIVE indicator of attrition.

Thus, we can arrive at a segment of employees with the following characteristics who have a high chance of attrition. 

POSITVIE - NumCompaniesWorked, YearsSinceLastPromotion, BusinessTravel.xTravel_Frequently, MaritalStatus.xSingle, WorkLifeBalance.x1, JobSatisfaction.x1, JobSatisfaction.x2, JobSatisfaction.x3, EducationField.xHuman.Resources

NEGATIVE - Age, TotalWorkingYears, YearsWithCurrManager, avg_time

FOR the beLow FACTORS, THE EDA ANAYLYSIS DONE PREVIOUSLY ON THEM WILL HELP US UNDERSTAND THE TREND THEY HAVE ON ATTRITION. THUS, WE CAN SAFELY CONCLUDE THAT,
	NumCompaniesWorked -  Employees who worked in  <1 and between  5 and 8 companies will have more attrition chances.
	YearsSinceLastPromotion – this factor has a standard positive effect, and hence nothing that substantially concluded from it.
	Age – Employees who are younger are prone to more attrition levels, excluding few exceptions
	TotalWorkingYears – employees with less working years are more like to attrition
	YearsWithCurrManager – higher years spent with the current manager tend to retain the employee
	avg_time – higher average time spent daily in the company indicates that the employee is satisfied and dedicated to his/her job and tends to leave the company less

Thus this model suggest a typical employee who has the highest chances of attrition having the following characteristics:
	worked in  <1 or between  5-8 no. of companies
	young age (<35 Years)
	Less working years/ experience (<10 Years)
	Less years spent with current manager (<8 Years)
	Whose daily average time spent in the company is low
	Travels frequently
	Martial status is single
	Work life balance is ‘bad’
	Job satisfaction is anything less than ‘very high’
	belongs to human resources field of education
Thus, employees belonging to this segments or having traits similar to this segment should be targeted the most to curb attrition'