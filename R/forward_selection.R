library()
#library(DAAG) # For cross validation


get_criterion <- function(data.frame) {
	linear.model <- lm(data.frame)
	lm.summary <- summary(linear.model)
	r.squared <- lm.summary[8]
	return(r.squared)
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

	fs.columns <- c(y.column.name) # Forward selection column vector
	for(i in 1:length(x.column.names)) {
		func.values <- vector()
		for(j in 1:length(x.column.names)) {
			new.column.vector <- fs.columns
			new.column.vector <- append(fs.columns, x.column.names[j])
			func.values <- append(func.values, get_criterion(file[,new.column.vector])) # Replace j with func(new.column.vector) where 'func' will return the criterion value	
			print(new.column.vector)
		}
		element = x.column.names[which.max(func.values)]	
		fs.columns <- append(fs.columns, element)
		x.column.names <- x.column.names[x.column.names != element] 
	}
}

main()