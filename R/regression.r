
# To do list of regressions :- 

# Regression
# Regression - WLS
# Ridge Regression
# Lasso Regression
# Quad Regression
# Response Surface

#understand a few details about every regression while executing

#install.packages("bindrcpp")  
library("bindrcpp")

#install.packages("dplyr") 
library("dplyr")

#install.packages('MASS')
library('MASS')

#install.packages('olsrr')
library('olsrr')

#install.packages('DAAG')
library(DAAG)

library(Hmisc)
library(psych)
library(car)

#install.packages('tidyverse')
library(tidyverse)
#install.packages('caret')
library(caret)

#install.packages('leaps')
library(leaps)
library(MASS)

#install.packages('nlme')
library(nlme)

library(lattice)

#install.packages('MuMIn')
library(MuMIn)

#setting working directory
#how to set dynamically to source file location??

setwd("~/sem2/ds2/Project1/Dataset1-autompg")

#load data 
mydata <- read.csv(file="../../CSCI-6360-Project01-Regression/data/1.csv", header=TRUE)
dim(mydata)
head(mydata, n=10)


#checking type of each column, which comes out to be double
sapply(mydata, typeof)

#doing mean imputation
data_impute_mean <- data.frame(
  sapply(
    mydata,
    function(x) ifelse(is.na(x),
                       mean(x, na.rm = TRUE),
                       x)))
head(data_impute_mean , n=20)

#doing forward selection using stepAIC library

fit2 <- lm(as.formula(paste(colnames(data_impute_mean)[1], "~",
                            paste('.', collapse = "+"),
                            sep = "")), data=data_impute_mean)

stepAIC(fit2,direction="forward")


# another forward selection method 

fitAll<-lm(as.formula(paste(colnames(data_impute_mean)[1], "~",
                            paste('.', collapse = "+"),
                            sep = "")), data=data_impute_mean)
summary(fitAll)
fitStart<-lm(as.formula(paste(colnames(data_impute_mean)[1], "~",
                               paste('1', collapse = "+"),
                               sep = "")), data=data_impute_mean)
summary(fitStart)

model4= step(fitStart, direction="forward",scope=formula(fitAll))



#the plan is to get info about what columns were implemented and then geenrate rcv square from it with for loop...

help(step)

# another pretty simple forward selection only needs package oslrr

warnings()

model <- lm(as.formula(paste(colnames(data_impute_mean)[1], "~",
                             paste('.', collapse = "+"),
                             sep = "")), data=data_impute_mean)


k <- ols_step_forward_p(model)

k$model


help("ols_step_forward_p")


model <- lm(as.formula(paste(colnames(data_impute_mean)[1], "~",
                             paste('.', collapse = "+"),
                             sep = "")), data=data_impute_mean)

#for cross validation and forward selection both

model2 <- cv.lm(data=data_impute_mean,k$model, m=10,seed=29) # 10 fold cross-validation

#model3<- CVlm(data =data_impute_mean, model, m=10, dots=FALSE, seed=29, legend.pos="topleft",  printit=FALSE)


plot(k)

#cross validation not applied on forward selection it seems. as model 2 input gives error in k..


help(lm)

#R2, R^2,R

modelSummary <- summary(model2)

attr(modelSummary, 'ms') 


# using cross validation with forward selection in training 

full.model <- lm(as.formula(paste(colnames(data_impute_mean)[1], "~",
                                  paste('.', collapse = "+"),
                                  sep = "")), data=data_impute_mean)

step.model1 <- stepAIC(full.model, direction = "forward", trace = FALSE)
#formula(step.model)
#summary(full.model)
summary(step.model1)
step.model1$bestTune
summary(step.model$finalModel1)
#models <- regsubsets(mpg~., data = autoMpg, nvmax = 5,method = "seqrep")
#summary(models)

#,tuneGrid = data.frame(nvmax = 1:15)
set.seed(123)

# Set up repeated k-fold cross-validation
train.control <- trainControl(method = "cv", number = 10)
# Train the model
step.model <- train(as.formula(paste(colnames(data_impute_mean)[1], "~",
                                     paste('.', collapse = "+"),
                                     sep = "")), data = data_impute_mean, method = "leapForward",trControl = train.control)

step.model$results

step.model$bestTune

summary(step.model$finalModel)

formula(step.model)

