# Correlation Lineplot
require(lattice)

correlation_lineplot <- function(x,y, method = 'pearson', main = 'Correlations') {
	
	correlationMatrix <- cor(x,y, method = method)
	correlationMatrix.m = melt(correlationMatrix)
	xyplot(abs(value) ~ X1, data <- correlationMatrix.m, groups = X2, type = c('a','g'), auto.key = list( column = 4), main = main)
	
	
	
	
}