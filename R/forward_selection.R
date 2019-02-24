library()
library(ggplot2)
library(caret)
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


# Returns rSq value for a linear regression model 
get_criterion <- function(data.frame) {
	linear.model <- lm(data.frame)
	model<-train(as.formula(paste(colnames(data.frame)[1], "~",
	                       paste('.', collapse = "+"),
	                       sep = "")), data = data.frame, method = "lm")
	
	r.squared<-model$results$Rsquared
	print(model$results)
	#translate below using train method
	lm.summary <- summary(linear.model)
	rss <- lm.summary[[7]][2] # 7th parameter in Summary is Residual Sum Squares (RSS)
	r.squared <- lm.summary[[8]] # 8th parameter in Summary is rSq
	adj.r.squared <- lm.summary[[9]]
	rsq.vector <- c(rss, 100*r.squared, 100*adj.r.squared)
	return(rsq.vector)
}

cross.validation <- function(data.frame, choice) {
	train.control <-trainControl(method = "cv", number = 10)
	if(choice == "1") {
		cv.results <- train(mpg ~ ., data=data.frame, method="lm", trControl= train.control)	
	} else if(choice == "2") {
		cv.results <- train(mpg ~ ., data=data.frame, method="ridge", trControl= train.control)
	} else if(choice == "3") {
		cv.results <- train(mpg ~ ., data=data.frame, method="lasso", trControl= train.control)
	} else if(choice == "4") {
		cv.results <- train(mpg ~ ., data=data.frame, method="lm", trControl= train.control)  # The dataframe will be different for Quad Regression
	} else if(choice == "5") {
		cv.results <- train(mpg ~ ., data=data.frame, method="lm", trControl= train.control) # The dataframe will be different for Response Surface
	} else {
		print("Invalid choice.")
	}
	return(100*cv.results$results$Rsquared)
}

main <- function() {
	#file <- read.table("D:/Spring2019/DataScienceII/Projects/CSCI-6360-Project01/data/1.csv", header=TRUE, sep=",")
    #file <- read.csv(file="C:/Users/Jayant/Documents/sem2/ds2/CSCI-6360-Project01-Regression/data/1.csv", header=TRUE, sep=",")
	file <- data.frame(sapply(file, function(x) ifelse(is.na(x), mean(x, na.rm = TRUE), x)))  # Mean Imputation
	column.names <- colnames(file) 
	y.column.name <- column.names[1]
	x.column.names <- column.names[2:length(column.names)]
	model.choice <- readline(prompt="1. Linear Regression\n2. Ridge Regression\n3. Lasso Regression\n4. Quad Regression\n5. Response Surface\n\n\tChoose model: ")

	# Forward Selection
	fs.columns <- c(y.column.name) # Forward selection column vector
	rSq <- vector()
	adj.rSq <- vector()
	rSqCv <- vector()
	for(i in 1:length(x.column.names)) {
		model.rss.values <- vector() # Vector to track RSS values
		model.rsq.values <- vector() # Vector to track rSq values
		model.adj.rsq.values <- vector() # Vector to track adjusted rSq values
		for(j in 1:length(x.column.names)) {
			new.column.vector <- fs.columns
			new.column.vector <- append(fs.columns, x.column.names[j])
			model.values <- get_criterion(file[,new.column.vector])
			model.rss.values <- append(model.rss.values, model.values[1])
			model.rsq.values <- append(model.rsq.values, model.values[2])
			model.adj.rsq.values <- append(model.adj.rsq.values, model.values[3])
		}
		min.index <- which.min(model.rss.values) # Column name with minimum RSS value
		fs.columns <- append(fs.columns, x.column.names[min.index]) # Forward selection column vector
		x.column.names <- x.column.names[x.column.names != x.column.names[min.index]] # Removing 'min.index' from column names vector
		adj.rSq <- append(adj.rSq, model.adj.rsq.values[min.index]) # Adding adjusted rSq value for column with maximum critrion
		rSq <- append(rSq, model.rsq.values[min.index]) # Adding rSq value for column with maximum criterion
		if(model.choice == "4") {

		} else if(model.choice == "5") {

		}
		else 
		cv.data.frame <- file[,fs.columns] 
		rSqCv <- append(rSqCv, cross.validation(cv.data.frame, model.choice)) # Adding rSqCV value for forward selection vector
	}
	#Plotting rSq and rSqCV
	plot(rSq, type = 'l', col = 'red', main = "Linear Regression", ylab = "Percentage", ylim = c(0,100))
	lines(adj.rSq,  col = 'green')
	lines(rSqCv,  col = 'blue' )
	legend(1,95, legend = c("R-Squared","Adjusted R-Squared", "R=Squared CV"), col = c("red","green", "blue"), lty = 1:2, cex = 0.8)
}

main()
