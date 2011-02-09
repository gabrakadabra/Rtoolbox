#-----------------------------------------------------------------------------------------------------------------------------------------------
# Beroenden
require(ggplot2)


# PCA Biplot
pca_biplot <- function(x, choices = 1L:2L, main = 'PCA', plot.scores = TRUE,...) { 
	argList<-list(...)

	# Handle input
	if (length(choices) != 2L) 
	stop("length of choices must be 2")
	if (!length(scores <- x$scores)) 
	stop(gettextf("object '%s' has no scores", deparse(substitute(x))), 
	domain = NA)

	# Set up scores and loadings
	scores = x$scores[,choices]
	scores = as.data.frame(scores,)
	loads = loadings(x)[,choices]

	# scaling part
	lam <- x$sdev[choices]
	if (is.null(n <- x$n.obs)) 
	n <- 1
	lam <- lam * sqrt(n)
	scores = as.data.frame(t(t(scores)/lam))


	# as.data.frame(FAP_Functional.pc$loadings[])
	
	loads = as.data.frame(loads[]/sd(loads)[1])
	print(loads)

	if( !is.null(argList$load.scale)) {
		load.scale = argList$load.scale
		# loads = as.data.frame(t(t(loads)*lam*load.scale))
		loads = as.data.frame(t(t(loads)*loadscale))
	}

	# Trick to make plot work
	names(loads) <- c('Comp.1','Comp.2')
	names(scores) <- c('Comp.1','Comp.2')

	# Add Rownames as a column
	loads$labels = rownames(loads)

	# Get the proportions of variace for labels and title
	prop_var <- x$sdev^2/sum(x$sdev^2)
	cum_var <- sum(prop_var[[choices[1]]],prop_var[[choices[2]]]) # The sum of variances from both components
	prop1 <- paste(',',format(prop_var[[choices[1]]], digits = 4))
	prop2 <- paste(',',format(prop_var[[choices[2]]], digits = 4))

	# paste('Canonical scores, FULL, cor = ',format(cc_full$cor[1],digits = 4))


	# The plottin part
	require(ggplot2); 
	# quartz()
	p <- ggplot(data = scores,aes(0,0))

	# Add Arrows
	p <- p + geom_segment(data = loads, aes(xend = Comp.1,yend = Comp.2),arrow=arrow(length=unit(0.2,"cm")),color='red',alpha = I(1/3),size = 1) 	# Plot loadings Arrows

	# Add scores and colouring of scores
	if(plot.scores){
		if( is.null(argList$colour)){
			p <- p + geom_point(aes(x = Comp.1,y = Comp.2),data = scores,alpha = I(2/3))
		}
		else{
			scores$color = argList$colour  
			p <- p + geom_point(aes(x = Comp.1,y = Comp.2, colour = color),data = scores,alpha = I(2/3))
		}
	}

	# shape
	if( !is.null(argList$shape)){
		p <- p + geom_shape

	}

	# textLabels and colouring
	if( !is.null(argList$text.labels)){
		if( !is.null(argList$colour)){
			scores$color = argList$colour
		}  
		scores$text.labels = argList$text.labels
		p <- p + geom_text(aes(x = Comp.1,y = Comp.2, label = text.labels, colour = color),data = scores)
	}

	# Rug
	if( !is.null(argList$colour)){
		p <- p + geom_rug(data= scores, aes(x = Comp.1, y = Comp.2, colour = color))
	}

	# Ellipse
	if( !is.null(argList$ellipse)){
		ellip <- ellipsoid(.1,.2,100)
		print(ellip)
		p <- p + geom_point(data <- ellip, mapping <- aes(x = x, y = y))

	}

	p <- p + 
	geom_text(data = loads, aes(x = Comp.1*1.1,y = Comp.2*1.1, label = labels ), angle=30) +	# Text till loadings
	geom_hline(yintercept = 0,colour = 'grey50') +					# Axlar genom origo
	geom_vline(xintercept = 0,colour = 'grey50') +
	scale_x_continuous(paste("Comp.",choices[1],prop1, sep = '')) + # Set up axis labels
	scale_y_continuous(paste("Comp.",choices[2],prop2, sep = '')) + 	
	coord_equal( ratio = 1) +											# locks the aspect ratio so that you can't skew your data when adjusting the window size
	theme_bw() + 													# BW theme for 
	opts(title = paste(main, format(cum_var, digits = 4), sep = ', '))
	return(p)
	p
}