library()
library(ggplot2)
#ibrary(DAAG) # For cross validation


# Returns rSq value for a linear regression model 
get_criterion <- function(data.frame) {
	linear.model <- lm(data.frame)
	lm.summary <- summary(linear.model)
	rss <- lm.summary[[7]][2] # 7th parameter in Summary is Residual Sum Squares (RSS)
	r.squared <- lm.summary[[8]] # 8th parameter in Summary is rSq
	adj.r.squared <- lm.summary[[9]]
	rsq.vector <- c(rss, 100*r.squared, 100*adj.r.squared)
	return(rsq.vector)
}

main <- function() {
	file <- read.table("D:/Spring2019/DataScienceII/Projects/CSCI-6360-Project01/data/1.csv", header=TRUE, sep=",")
	dimensions <- dim(file) 
	column.names <- colnames(file) 
	y.column.name <- column.names[1]
	x.column.names <- column.names[2:length(column.names)]

	# Mean Imputation
	#for(col in colnames(file)) {
	#	file$col[is.na(file$col)] <- mean(file$col, na.rm = TRUE)
	#}

	# Forward Selection
	fs.columns <- c(y.column.name) # Forward selection column vector
	rSq <- vector()
	adj.rSq <- vector()
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
		fs.columns <- append(fs.columns, x.column.names[min.index])
		x.column.names <- x.column.names[x.column.names != x.column.names[min.index]] # Removing 'min.index' from column names vector
		adj.rSq <- append(adj.rSq, model.adj.rsq.values[min.index])
		rSq <- append(rSq, model.rsq.values[min.index]) # Adding rSq value for column with maximum criterion

	}
	#Plotting rSq and rSqCV
	plot(rSq, type = 'l', col = 'red', main = "Linear Regression", ylab = "Percentage", ylim = c(0,100))
	lines(adj.rSq,  col = 'green')
	#lines(rSqCV,  col = 'blue' )
	legend(1,95, legend = c("R Squared","Adj R Squared"), col = c("red","green"), lty = 1:2, cex = 0.8)
}

main()

#cross validation function

k_fold_rsq <- function(lmfit, ngroup=10) {
  # assumes library(bootstrap)
  # adapted from http://www.statmethods.net/stats/regression.html
  mydata <- lmfit$model
  outcome <- names(lmfit$model)[1]
  predictors <- names(lmfit$model)[-1]
  
  theta.fit <- function(x,y){lsfit(x,y)}
  theta.predict <- function(fit,x){cbind(1,x)%*%fit$coef} 
  X <- as.matrix(mydata[predictors])
  y <- as.matrix(mydata[outcome]) 
  
  results <- crossval(X,y,theta.fit,theta.predict,ngroup=ngroup)
  raw_rsq <- cor(y, lmfit$fitted.values)**2 # raw R2 
  cv_rsq <- cor(y,results$cv.fit)**2 # cross-validated R2
  
  c(raw_rsq=raw_rsq, cv_rsq=cv_rsq)
}