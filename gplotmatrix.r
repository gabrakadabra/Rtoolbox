#-----------------------------------------------------------------------------------------------------------------------------------------------
# Beroenden
require(ggplot2)
require(ellipse)

# Advanced plot matrix
gplotmatrix <- function(x, points = TRUE, groupcolour = c(1,2), ...) {
	# arguments <- list(...)
	
	
	x <- as.data.frame(x)
	# print(gcolour)
	# create the dummy plot
	theplot <- plotmatrix(x, colour = rgb(1,1,1,0))
	
	# data dimensions
	n <- nrow(theplot$data)
	gcolour=rep(groupcolour, length.out = n)
	
	# add the points
	if(points){
		theplot <- theplot + 
		geom_point(aes(colour = gcolour),
		size = 4, alpha = 0.8, shape = 1) + 
		scale_colour_hue('apa')
	}
	
	
	
	print(theplot)
}
gplotmatrix(iris[,1:3], groupcolour = ')