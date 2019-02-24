library()
library(ggplot2)
library(caret)
library(lattice)
#library(glmnet)


# Returns rSq value for a linear regression model 
get_adj_rSq <- function(r.squared, n, p) {
	rsq.cv <- 1 - (1 - r.squared)*(n - 1) / (n - p - 1)
	return(rsq.cv)
}

# Returns rSquared value based on minimum RSS/ maximum rSquared
get_rSq <- function(data.frame, choice) {
	if(choice == "1") {
		model <- train(as.formula(paste( names(data.frame)[1], "~", paste('.', collapse = "+"),sep = "")), 
						data=data.frame, method="lm")	
	} else if(choice == "2") {
		model <- train(as.formula(paste(colnames(data.frame)[1], "~", paste('.', collapse = "+"),sep = "")), 
							data=data.frame, method="ridge")
	} else if(choice == "3") {
		model <- train(as.formula(paste(colnames(data.frame)[1], "~", paste('.', collapse = "+"),sep = "")), 
							data=data.frame, method="lasso")
	} else if(choice == "4") {
		model <- train(as.formula(paste(colnames(data.frame)[1], "~", paste('.', collapse = "+"),sep = "")), 
							data=data.frame, method="lm")
	} else if(choice == "5") {
		model <- train(as.formula(paste(colnames(data.frame)[1], "~", paste('.', collapse = "+"),sep = "")), 
							data=data.frame, method="lm") 
	} else {
		print("InvalidChoice: Please enter choice between '1' and '5'")
	}
	r.squared <- model$results$Rsquared
	return(100*r.squared)
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
	file <- read.table("D:/Spring2019/DataScienceII/Projects/CSCI-6360-Project01/data/1.csv", header=TRUE, sep=",")
    #file <- read.csv(file="C:/Users/Jayant/Documents/sem2/ds2/CSCI-6360-Project01-Regression/data/1.csv", header=TRUE, sep=",")
	imp.file <- data.frame(sapply(file, function(x) ifelse(is.na(x), mean(x, na.rm = TRUE), x)))  # Mean Imputation
	set.seed(123)
	shuffled.file <- imp.file[sample(nrow(imp.file)), ]
	split <- as.integer(0.6 * as.integer(nrow(shuffled.file)))																			
	train.file <- shuffled.file[1 : split, ]
	test.file <- shuffled.file[split : nrow(shuffled.file), ]

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
			model.rsq.values <- append(model.rsq.values, get_rSq(train.file[,new.column.vector], model.choice))
		}
		max.index <- which.max(model.rsq.values) # Column name with maximum rSquared value
		fs.columns <- append(fs.columns, x.column.names[max.index]) # Forward selection column vector
		x.column.names <- x.column.names[x.column.names != x.column.names[max.index]] # Removing 'max.index' from column names vector
		fs.col.rSq <- get_rSq(test.file[, fs.columns], model.choice)
		test.data.n <- nrow(test.file)
		test.data.p <- ncol(test.file) - 1
		adj.rSq <- append(adj.rSq, get_adj_rSq(fs.col.rSq, test.data.n, test.data.p)) # Adding adjusted rSq value for column with maximum critrion
		rSq <- append(rSq, fs.col.rSq) # Adding rSq value for column with maximum criterion
		if(model.choice == "4") {

		} else if(model.choice == "5") {

		}
		else {
			cv.data.frame <- shuffled.file[,fs.columns] 
			rSqCv <- append(rSqCv, cross_validation(cv.data.frame, model.choice)) # Adding rSqCV value for forward selection vector
		}
	}
	#Plotting rSq and rSqCV
	plot(rSq, type = 'l', col = 'red', main = "Regression Problem in R", ylab = "Percentage", ylim = c(0,100))
	lines(adj.rSq,  col = 'green')
	lines(rSqCv,  col = 'blue' )
	legend(1,95, legend = c("R-Squared","Adjusted R-Squared", "R=Squared CV"), col = c("red","green", "blue"), lty = 1:2, cex = 0.8)
}
 
main()
