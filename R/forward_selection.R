library(ggplot2)
library(caret)
library(lattice)



# Returns rSq value for a linear regression model 
get_adj_rSq <- function(r.squared, n, p) {
	rsq.cv <- 1 - (1 - r.squared)*(n - 1) / (n - p - 1)
	return(rsq.cv)
}

# Returns rSquared value based on minimum RSS/ maximum rSquared
get_criterion <- function(data.frame) {

	model <- train(as.formula(paste(colnames(data.frame)[1], "~", paste('.', collapse = "+"),sep = "")), 
					data = data.frame, method = "lm")
	r.squared <- model$results$Rsquared
	p <- ncol(data.frame) - 1
	n <- nrow(data.frame)
	adj.r.squared <- get_adj_rSq(r.squared, n, p)
	rsq.vector <- c(100*r.squared, 100*adj.r.squared)

	return(rsq.vector)
}

# Performs cross validation on input data frame
cross_validation <- function(data.frame, choice) {
	train.control <-trainControl(method = "cv", number = 10)
	if(choice == "1") {
		cv.results <- train(as.formula(paste(colnames(data.frame)[1], "~", paste('.', collapse = "+"),sep = "")), 
							data=data.frame, method="lm", trControl= train.control)	
	} else if(choice == "2") {
		cv.results <- train(as.formula(paste(colnames(data.frame)[1], "~", paste('.', collapse = "+"),sep = "")), 
							data=data.frame, method="ridge", trControl= train.control)
	} else if(choice == "3") {
		cv.results <- train(as.formula(paste(colnames(data.frame)[1], "~", paste('.', collapse = "+"),sep = "")), 
							data=data.frame, method="lasso", trControl= train.control)
	} else if(choice == "4") {
		cv.results <- train(as.formula(paste(colnames(data.frame)[1], "~", paste('.', collapse = "+"),sep = "")), 
							data=data.frame, method="lm", trControl= train.control)  # The dataframe will be different for Quad Regression
	} else if(choice == "5") {
		cv.results <- train(as.formula(paste(colnames(data.frame)[1], "~", paste('.', collapse = "+"),sep = "")), 
							data=data.frame, method="lm", trControl= train.control) # The dataframe will be different for Response Surface
	} else {
		print("InvalidChoice: Please enter choice between '1' and '5'")
	}
	return(100*cv.results$results$Rsquared)
}

# Main function
main <- function() {
	#file <- read.table("D:/Spring2019/DataScienceII/Projects/CSCI-6360-Project01/data/1.csv", header=TRUE, sep=",")
  file <- read.csv(file="C:/Users/Jayant/Documents/sem2/ds2/CSCI-6360-Project01-Regression/data/1.csv", header=TRUE, sep=",")
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
			model.rsq.values <- append(model.rsq.values, model.values[1])
			model.adj.rsq.values <- append(model.adj.rsq.values, model.values[2])

		}
		max.index <- which.max(model.rsq.values) # Column name with minimum RSS value
		fs.columns <- append(fs.columns, x.column.names[max.index]) # Forward selection column vector
		x.column.names <- x.column.names[x.column.names != x.column.names[max.index]] # Removing 'min.index' from column names vector
		adj.rSq <- append(adj.rSq, model.adj.rsq.values[max.index]) # Adding adjusted rSq value for column with maximum critrion
		rSq <- append(rSq, model.rsq.values[max.index]) # Adding rSq value for column with maximum criterion
		if(model.choice == "4") {
		  #quad regression
		  cv.data.frame <- file[,fs.columns]
		  for(column in fs.columns[2:length(fs.columns)]) {
		    new.column = paste(column,'^2')
		    cv.data.frame[, new.column] <- cv.data.frame[, column] * cv.data.frame[, column]
		  }
		  rSqCv <- append(rSqCv, cross_validation(cv.data.frame, model.choice)) # Adding rSqCV value for forward selection vector
		} else if(model.choice == "5") {

		}
		else {
			cv.data.frame <- file[,fs.columns] 
			rSqCv <- append(rSqCv, cross_validation(cv.data.frame, model.choice)) # Adding rSqCV value for forward selection vector
		}
	}
	#Plotting rSq and rSqCV
	plot(rSq, type = 'l', col = 'red', main = "Linear Regression", ylab = "Percentage", ylim = c(0,100))
	lines(adj.rSq,  col = 'green')
	lines(rSqCv,  col = 'blue' )
	legend(1,95, legend = c("R-Squared","Adjusted R-Squared", "R=Squared CV"), col = c("red","green", "blue"), lty = 1:2, cex = 0.8)
}

main()
