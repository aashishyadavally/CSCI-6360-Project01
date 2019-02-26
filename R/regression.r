#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#** @author  Aashish Yadavally; Jayant Parashar
#*  @version 1.0
#*  @date    Feb 25, 2019
#*  @see     LICENSE (MIT style license file)
#*/

library(ggplot2)
library(caret)
library(lattice)
library(lmridge)
library(lars)


# Returns rSq value for a linear regression model 
get_adj_rSq <- function(r.squared, n, p) {
	rsq.cv <- 1 - (1 - r.squared)*(n - 1) / (n - p - 1)
	return(rsq.cv)
}

# Returns rSquared value based on minimum RSS/ maximum rSquared
get_rSq <- function(data.frame, choice) {
	if(choice == "1") {	# Simple Regression
		model <- lm(as.formula(paste(colnames(data.frame)[1], "~", paste('.', collapse = "+"),sep = "")), data=data.frame)
		r.squared <- summary(model)[[8]]
	} else if(choice == "2") { # Ridge Regression
		if(as.integer(dim(data.frame)[2]) <=3) {	# Requires atleast two features in 'x' matrix
			r.squared <- 0
		} else {
			model <- lmridge(as.formula(paste(colnames(data.frame)[1], "~", paste('.', collapse = "+"),sep = "")), data = data.frame, K = c(0.1, 0.001))
			r.squared <- max(rstats1(model)$R2)
		}
	} else if(choice == "3") {	# Lasso Regression
		if(as.integer(dim(data.frame)[2]) <=3) {	# Requires atleast two features in 'x' matrix
			r.squared <- 0
		} else {
			x <- as.matrix(data.frame[, 2:dim(data.frame)[2]])
			y <- as.vector(data.frame[, 1])
			model <- lars(x, y, type = 'lasso')
			r.squared <- max(model$R2)
		}
	} else if(choice == "4") {	# Quad Regression
		model <- lm(as.formula(paste(colnames(data.frame)[1], "~", paste('.', collapse = "+"),sep = "")), data=data.frame)
		r.squared <- summary(model)[[8]]
	} else if(choice == "5") {	# Response Surface
		model <- lm(as.formula(paste(colnames(data.frame)[1], "~", paste('.', collapse = "+"),sep = "")), data=data.frame) 
		r.squared <- summary(model)[[8]]
	} else {
		print("InvalidChoice: Please enter choice between '1' and '5'.")
	}	
return(100*r.squared)
}

# Performs cross validation on input data frame
cross_validation <- function(data.frame, choice) {
	train.control <-trainControl(method = "cv", number = 10)
	if(choice == "1") {	# Simple Regression
		cv.results <- train(as.formula(paste(colnames(data.frame)[1], "~", paste('.', collapse = "+"),sep = "")), 
							data=data.frame, method="lm", trControl= train.control)
		return.value <- 100*cv.results$results$Rsquared	
	} else if(choice == "2") {	# Ridge Regression
		if(as.integer(dim(data.frame)[2]) <=3){	# Requires atleast two features in X-matrix
			return.value <- 0
		} else {
		cv.results <- train(as.formula(paste(colnames(data.frame)[1], "~", paste('.', collapse = "+"),sep = "")), 
							data=data.frame, method="ridge", trControl= train.control)
		return.value <- 100*cv.results$results$Rsquared
		}
	} else if(choice == "3") {	# Lasso Regression
		if(as.integer(dim(data.frame)[2]) <=3){	# Requires atleast two features in X-matrix
			return.value <- 0
		} else {
		cv.results <- train(as.formula(paste(colnames(data.frame)[1], "~", paste('.', collapse = "+"),sep = "")), 
							data=data.frame, method="lasso", trControl= train.control)
		return.value <- 100*cv.results$results$Rsquared
		}
	} else if(choice == "4") {	# Quad Regression
		cv.results <- train(as.formula(paste(colnames(data.frame)[1], "~", paste('.', collapse = "+"),sep = "")), 
							data=data.frame, method="lm", trControl= train.control)  # The dataframe will be different for Quad Regression
		return.value <- 100*cv.results$results$Rsquared
	} else if(choice == "5") {	# Response Surface
		cv.results <- train(as.formula(paste(colnames(data.frame)[1], "~", paste('.', collapse = "+"),sep = "")), 
							data=data.frame, method="lm", trControl= train.control) # The dataframe will be different for Response Surface
		return.value <- 100*cv.results$results$Rsquared
	} else {
		print("InvalidChoice: Please enter choice between '1' and '5'.")
	}
	return(return.value)
}

# Main function
main <- function() {
	cat(" Select dataset: \n\t 1. Auto MPG \n\t 2. Beijing PM2.5 Dataset \n\t 3. Concrete Compressive Strength Dataset \n\t 4. Real Estate Valuation Dataset \n\t 5. Parkinson's Tele Monitoring \n\t 6. Computer Hardware")
	cat("\n\t 7. Appliances Energy Prediction  \n\t 8. Combined Cycle Powerplant \n\t 9. CSM Dataset \n\t 10. Naval Propulsion Dataset \n\t 11. For other datasets, enter: /correct/path/to/data/csv\n")
	dataset.choice <- readline(prompt="\t\tEnter your choice:")
	
	if(as.integer(dataset.choice)>0 && as.integer(dataset.choice)<11) {
		path <- paste("D:/Spring2019/DataScienceII/Projects/CSCI-6360-Project01/data/", dataset.choice, ".csv", sep="")
	}
	else if(as.integer(dataset.choice)==11) {path <- readline(prompt="Enter correct path of your dataset: ")}
	else {
		print("Invalid Choice: Please enter choice between '1' and '11'.")
		quit()
	}
	
	file <- read.table(file=path, header=TRUE, sep=",")
	imp.file <- data.frame(sapply(file, function(x) ifelse(is.na(x), mean(x, na.rm = TRUE), x)))  # Mean Imputation
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
			model.rsq.values <- append(model.rsq.values, get_rSq(imp.file[,new.column.vector], model.choice))
		}
		max.index <- which.max(model.rsq.values) # Column name with maximum rSquared value
		fs.columns <- append(fs.columns, x.column.names[max.index]) # Forward selection column vector
		x.column.names <- x.column.names[x.column.names != x.column.names[max.index]] # Removing 'max.index' from column names vector
		
		# R-Squared and Adjusted R-Squared on Test set; Cross validation on full dataframe
		# If model choice is 4, column names corresponding to Quad Regression are attached to dataframe
		# If model choice is 5, column names corresponding to Response Surface are attached to dataframe
		# Else, other models are implemented, which require a Linear Model, but for model choice 2 and 3,
		# lambda regularization parameter is added.
		if(model.choice == "4") {
			cv.data.frame <- imp.file[,fs.columns]
			for(column in fs.columns[2:length(fs.columns)]) {
				new.column = paste(column,'^2')
				cv.data.frame[, new.column] <- cv.data.frame[, column] * cv.data.frame[, column]
			}
			data.n <- nrow(cv.data.frame)
			data.p <- ncol(cv.data.frame) - 1
  			rSq <- append(rSq, get_rSq(cv.data.frame, model.choice)) # Adding rSq value for column with maximum criterion
			adj.rSq <- append(adj.rSq, get_adj_rSq(rSq[i], data.n, data.p)) # Adding adjusted rSq value for column with maximum critrion
			rSqCv <- append(rSqCv, cross_validation(cv.data.frame, model.choice)) # Adding rSqCV value for forward selection vector
		} else if(model.choice == "5") {
			cv.data.frame <- imp.file[,fs.columns]
			for(i in 1:length(fs.columns)) {
				for(j in i:length(fs.columns)) {
					if(i==j) {new.column = paste(fs.columns[i],'^2')}
					else {new.column = paste(fs.columns[i], '_', fs.columns[j])}
					cv.data.frame[, new.column] <- cv.data.frame[, fs.columns[i]] * cv.data.frame[, fs.columns[i]]
				} 				
			}
			data.n <- nrow(cv.data.frame)
			data.p <- ncol(cv.data.frame) - 1
  			rSq <- append(rSq, get_rSq(cv.data.frame, model.choice)) # Adding rSq value for column with maximum criterion
			adj.rSq <- append(adj.rSq, get_adj_rSq(rSq[i], data.n, data.p)) # Adding adjusted rSq value for column with maximum critrion
			rSqCv <- append(rSqCv, cross_validation(cv.data.frame, model.choice)) # Adding rSqCV value for forward selection vector
		} else {
			cv.data.frame <- imp.file[,fs.columns]
			data.n	<- nrow(cv.data.frame)
			data.p <- ncol(cv.data.frame) - 1
 			rSq <- append(rSq, get_rSq(cv.data.frame, model.choice)) # Adding rSq value for column with maximum criterion
			adj.rSq <- append(adj.rSq, get_adj_rSq(rSq[i], data.n, data.p)) # Adding adjusted rSq value for column with maximum critrion		
			rSqCv <- append(rSqCv, cross_validation(cv.data.frame, model.choice)) # Adding rSqCV value for forward selection vector
		}
	}
	#Plotting rSq and rSqCV
	plot(rSq[1:length(rSq)], type = 'l', col = 'red', main = "Regression Problem in R", ylab = "Percentage", ylim = c(0,100))
	lines(adj.rSq[1:length(adj.rSq)],  col = 'green')
	lines(rSqCv[1:length(rSqCv)],  col = 'blue' )
	legend(1,95, legend = c("R-Squared","Adjusted R-Squared", "R=Squared CV"), col = c("red","green", "blue"), lty = 1:2, cex = 0.8)
}

main()
