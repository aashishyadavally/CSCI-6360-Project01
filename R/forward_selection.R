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
	#print(model$results)
	#translate below using train method
	model.summary <- summary(model)
	rss <- model.summary[[7]][2] # 7th parameter in Summary is Residual Sum Squares (RSS)
	r.squared <- model.summary[[8]] # 8th parameter in Summary is rSq
	adj.r.squared <- model.summary[[9]]
	rsq.vector <- c(rss, 100*r.squared, 100*adj.r.squared)
	return(rsq.vector)
}

main <- function() {
	#file <- read.table("D:/Spring2019/DataScienceII/Projects/CSCI-6360-Project01/data/1.csv", header=TRUE, sep=",")
  file <- read.csv(file="C:/Users/Jayant/Documents/sem2/ds2/CSCI-6360-Project01-Regression/data/1.csv", header=TRUE, sep=",")
	file <- data.frame(sapply(file, function(x) ifelse(is.na(x), mean(x, na.rm = TRUE), x)))  # Mean Imputation
	column.names <- colnames(file) 
	y.column.name <- column.names[1]
	x.column.names <- column.names[2:length(column.names)]

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
		cv.data.frame <- file[,fs.columns]
		train.control <-trainControl(method = "cv", number = 10)
		cross.validation <- train(mpg ~ ., data=cv.data.frame, method="lm", trControl= train.control)
		x.column.names <- x.column.names[x.column.names != x.column.names[min.index]] # Removing 'min.index' from column names vector
		adj.rSq <- append(adj.rSq, model.adj.rsq.values[min.index]) # Adding adjusted rSq value for column with maximum critrion
		rSq <- append(rSq, model.rsq.values[min.index]) # Adding rSq value for column with maximum criterion
		rSqCv <- append(rSqCv, 100*cross.validation$results$Rsquared) # Adding rSqCV value for forward selection vector
	}
	#Plotting rSq and rSqCV
	plot(rSq, type = 'l', col = 'red', main = "Linear Regression", ylab = "Percentage", ylim = c(0,100))
	lines(adj.rSq,  col = 'green')
	lines(rSqCv,  col = 'blue' )
	legend(1,95, legend = c("R-Squared","Adjusted R-Squared", "R=Squared CV"), col = c("red","green", "blue"), lty = 1:2, cex = 0.8)
}

main()
