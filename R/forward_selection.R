library()
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
	print('R-Squared for different values of n is: ')
	print(rSq)
	print('Adjusted R-Squared for different values of n is: ')
	print(adj.rSq)
}

main()