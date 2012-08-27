#-----------------------------------------------------------------------------------------------------------------------------------------------
# Beroenden
require(ggplot2)

# Screeplot i paretochart form
scree_pareto <- function(x, fillBars = TRUE, xlab = 'Components', scale = 1){ 
	# Input: x <- princomp(data)
	# Screeplot, för att se variansproportionen, ges av Egenvärderna summary(x)$sdev^2 = eigen(cor(x))

	sdev  <- as.data.frame(x$sdev^2/sum(x$sdev^2))
	names(sdev) <- 'dev'
	sdev$dev2 <- reorder(as.factor(rownames(sdev)),1:dim(sdev)[1]) #Fixa levels i rätt ordning
	sdev$cum <- cumsum(x$sdev^2/sum(x$sdev^2))

	main = paste('Screeplot', substitute(x))

	# Set up plot
	p <- ggplot(data = sdev, aes(x= dev2))
	if(fillBars){
		p <- p + geom_bar(aes(x = dev2, y = dev, fill = dev2))
		} 
	else{ p <- p + geom_bar(aes(x = dev2, y = dev), colour = 'grey20', fill = 'white')
		} 
	p <- p + geom_line(aes(y = cum, group = 1), colour = 'red') +
	geom_point(aes(y = cum,), colour = 'red') + 
	geom_hline(aes(yintercept = 0.95), colour = 'grey40', linetype = 2) + 
	opts(title = main)	+
	xlab( xlab ) + 
	ylab('Explained') + 
	theme_bw()
	
	# plot
	p
	# return plot
	return(p)
}
