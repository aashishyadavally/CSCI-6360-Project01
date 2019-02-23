library()
#library(DAAG) # For cross validation


file <- read.table("D:/Spring2019/DataScienceII/Projects/CSCI-6360-Project01/data/1.csv", header=TRUE, sep=",")
dimensions <- dim(file)
x <- file[, 2:n_cols]
y <- file[, 1]

# Mean Imputation
#for(col in colnames(file)) {
#	file$col[is.na(file$col)] <- mean(file$col, na.rm = TRUE)
#}

#column.names <- colnames(x)
column.names <- c('1','3','2','4') # Dummy column vector
fs.columns <- vector() # Forward selection column vector
for(i in 1:length(column.names)) {
	func.values <- vector()
	for(j in 1:length(column.names)) {
		new.column.vector <- fs.columns
		new.column.vector <- append(fs.columns, column.names[j])
		func.values <- append(func.values, j) # Replace j with func(new.column.vector) where 'func' will return the criterion value	
		print(new.column.vector)
	}
	element = which.max(func.values)	
	fs.columns <- append(fs.columns, element)
	column.names <- column.names[column.names != element] 
}
