#-----------------------------------------------------------------------------------------------------------------------------------------------
# Beroenden
require(ggplot2)

# Screeplot i paretochart form
scree_pareto <- function(x){ 
	# Screeplot, för att se variansproportionen, ges av Egenvärderna summary(x)$sdev^2 = eigen(cor(x))
	
	
	
	
	sdev  <- as.data.frame(x$sdev^2/sum(x$sdev^2))
	names(sdev) <- 'dev'
	sdev$dev2 <- reorder(as.factor(rownames(sdev)),1:dim(sdev)[1]) #Fixa levels i rätt ordning
	sdev$cum <- cumsum(x$sdev^2/sum(x$sdev^2))

	main = paste('Screeplot', substitute(x))

	# Plotting
	p <- ggplot(data = sdev, aes(x= dev2))
	p <- p + geom_bar(aes(x = dev2, y = dev, fill = dev2)) + 
	geom_line(aes(y = cum, group = 1), colour = 'red') +
	geom_point(aes(y = cum,), colour = 'red') + 
	geom_hline(aes(yintercept = 0.95), colour = 'grey40', linetype = 2) + 
	opts(title = main)	+
	xlab('Components') + 
	ylab('Explained') + 
	theme_bw()
	
	p
	return(p)
}