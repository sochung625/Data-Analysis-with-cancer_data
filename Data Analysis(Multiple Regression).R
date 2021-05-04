#-----------------------------------------Hypothesis Testing-------------------------------------#
install.packages("tidyverse")
library(tidyverse)

# Importing Data
Cancer_data <- read.csv("C:/Users/SEC/Desktop/cancer_reg_categorical.csv")
View(Cancer_data)
colnames(Cancer_data)

# Data Processing
head(Cancer_data)

# Converting Variables as factor
Cancer_data$State=as.factor(Cancer_data$State)
Cancer_data$Married=as.factor(Cancer_data$Married)
table(Cancer_data$State)
table(Cancer_data$Married)

# Fitting the Linear Regression Model
full.model <- lm(TARGET_deathRate~ ., data = Cancer_data)
full.model
summary(full.model)

# Setting all the linear models
married.removed.model <- lm(TARGET_deathRate ~ State + avgAnnCount + avgDeathsPerYear + incidenceRate + medIncome + popEst2015 + povertyPercent + studyPerCap + MedianAge + MedianAgeMale + MedianAgeFemale + AvgHouseholdSize + PctNoHS18_24 + PctHS18_24 + PctSomeCol18_24 + PctBachDeg18_24 + PctHS25_Over + PctBachDeg25_Over + PctEmployed16_Over + PctUnemployed16_Over + PctPrivateCoverage + PctPrivateCoverageAlone + PctEmpPrivCoverage + PctPublicCoverage + PctPublicCoverageAlone + PctWhite + PctBlack + PctAsian + PctOtherRace + BirthRate,data = Cancer_data)
state.removed.model <- lm(TARGET_deathRate ~ Married + avgAnnCount + avgDeathsPerYear + incidenceRate + medIncome + popEst2015 + povertyPercent + studyPerCap + MedianAge + MedianAgeMale + MedianAgeFemale + AvgHouseholdSize + PctNoHS18_24 + PctHS18_24 + PctSomeCol18_24 + PctBachDeg18_24 + PctHS25_Over + PctBachDeg25_Over + PctEmployed16_Over + PctUnemployed16_Over + PctPrivateCoverage + PctPrivateCoverageAlone + PctEmpPrivCoverage + PctPublicCoverage + PctPublicCoverageAlone + PctWhite + PctBlack + PctAsian + PctOtherRace + BirthRate,data = Cancer_data)

# ANOVA testing
anova(full.model, married.removed.model)
anova(full.model, state.removed.model)
#-------------------------------------------------------------------------------------------------#
#------------------------------------Multiple Linear Regression-----------------------------------#
install.packages("dplyr")
library(dplyr)
Cancer_data <- read.csv("C:/Users/ryans/Desktop/archive/cancer_reg.csv")
Cancer_data
colnames(Cancer_data)
cancer_data_linear <- lm((avgAnnCount)~ ., data = Cancer_data)
summary(cancer_data_linear)
install.packages("boxcoxmix")
install.packages("MASS")
library(MASS)
boxcox(cancer_data_linear)
plot(cancer_data_linear)
plot(Cancer_data)

# P-VALUE
install.packages("olsrr")
library(olsrr)
install.packages("car")
library(car)
#FULL MODEL
all_model <- lm(TARGET_deathRate~ ., data = Cancer_data)
summary(all_model)
vif(all_model)
#STEPWISE REGRESSION
stepWiseRegressionModel <-ols_step_both_p(all_model,pent = 0.05,prem = 0.05, detail = TRUE)
stepWiseRegressionModel
stepWiseRegressionModel$model
stepWiseRegressionModel$steps
stepWiseRegressionModel$rsquare
stepWiseRegressionModel$orders
plot(stepWiseRegressionModel)
#BACKWARD ELIMINATION
backwardEliminationModel <- ols_step_backward_p(all_model,prem=0.05, detail= TRUE)
plot(backwardEliminationModel)
backwardEliminationModel$model
backwardEliminationModel$indvar
backwardEliminationModel
#FORWARD ELIMINATION
forWardEliminationModel <-ols_step_forward_p(all_model,pent = 0.05,detail = TRUE)
forWardEliminationModel

#AKAIKI
step(all_model, direction = "forward")
step_back <- step(all_model, direction = "backward")
summary(step_back)
vif(step_back)
step_both <-step(all_model, direction = "both")
summary(step_both)
#--------------------------------------------------------------------------------------------------#
#---------------------------------------Effect of Missing Values-----------------------------------#
Cancer_data2 <- read.csv("cancer_reg.csv", header=TRUE)
library(car)
# Random Deletion
library(simFrame)
dataFrame <-as.data.frame(Cancer_data2)
nac<-NAControl(NArate=0.2) #set missing rate
Newdata<-setNA(dataFrame,nac) #get new data(with missing data)

# Imputation
library(mice)
data_incomplete = Newdata
imp = mice(data_incomplete, printFlag = FALSE)
meth = imp$meth
meth = "pmm"
Imp = mice(data_incomplete, maxit = 3, printFlag = FALSE, meth = meth)
Cancer_data22 = complete(Imp)
all_model_2 <- lm(TARGET_deathRate~ ., data = Cancer_data22)
vif(all_model_2)
drop <- c("PctPrivateCoverageAlone", "PctSomeCol18_24", "PctPublicCoverage", "PctPrivateCoverage", "PercentMarried", "popEst2015", "MedianAgeFemale", "povertyPercent", "PctWhite")
Cancer_data32 <- Cancer_data22[!(names(Cancer_data22) %in% drop)]
all_model_2 <- lm( TARGET_deathRate ~ ., data= Cancer_data32)
vif(all_model_2)
library(MASS) # For BoxCox Normality Check
boxcox(all_model_2)

# Regression using AIC
step_model1 <- step(all_model_2, direction = "forward")
summary(step_model1)

step_model2 <- step(all_model_2, direction = "backward")
summary(step_model2)

step_model3 <-step(all_model_2, direction = "both")
summary(step_model3)


library(olsrr) # Stepwise Regression

#Both Direction
step_model4 <- ols_step_both_p(all_model_2, pent = 0.05, prem = 0.05, detail = TRUE)
step_model4
step_model4$model

#Backward
step_model5 <- ols_step_backward_p(all_model_2, prem=0.05, detail= TRUE)
step_model5
step_model5$model

#Forward
step_model6 <-ols_step_forward_p(all_model_2,pent = 0.05,detail = TRUE)
step_model6
step_model6$model

Final_imp_model <- lm( TARGET_deathRate ~ 
                         avgAnnCount+
                         incidenceRate+
                         MedianAgeMale+
                         PctHS18_24+
                         PctHS25_Over+
                         PctBachDeg25_Over+
                         PctEmployed16_Over+
                         PctPublicCoverageAlone+
                         PctOtherRace+
                         PctMarriedHouseholds +
                         BirthRate+
                         avgDeathsPerYear, data = Cancer_data32)

summary( Final_imp_model )
#--------------------------------------------------------------------------------------------------