# Correlation plot.
# Ref	[1] http://sciblogs.co.nz/the-atavism/2009/05/27/plotting-a-correlation-matrix-with-ggplot2/
# 		[2] http://tolstoy.newcastle.edu.au/R/help/05/04/2659.html

require(ggplot2)

corplot <- function(x, subTitle = '', ...) {
	argList<-list(...)


	#Make the correlation matrix
	(x.s <- cor(x))

	# same with p-values, then use symnum() to represent the values as asterisks
	cor.pval <- function(x, alternative="two-sided", ...) {

		corMat <- cor(x, ...) 
		n <- nrow(x) 
		df <- n - 2 
		STATISTIC <- sqrt(df) * corMat / sqrt(1 - corMat^2)     
		p <- pt(STATISTIC, df)
		p <- if (alternative == "less") {

		} 
		else if (alternative == "greater") {

			1 - p 
		} 
		else 2 * pmin(p, 1 - p) 
		p 
	}
	p <- cor.pval(x)

	symnum(x.s)

	# melt the correlation data
	m <- melt(x.s)

	stars <- as.character(symnum(p, cutpoints=c(0,0.001,0.01,0.05,1),
	symbols=c('***', '**', '*', '' ),
	legend=F))

	m$stars = stars

	#now put them alltogether (with melt() to reshape the correlation matrix) molten.iris
	names(m) <- c("M1", "M2", "corr", "pvalue")

	# Define each triangle of the plot matric and the diagonal (mi.ids)
	mi.ids <- subset(m, M1 == M2) # diagonal
	mi.lower <- subset(m[lower.tri(x.s),], M1 != M2) # Upper
	mi.upper <- subset(m[upper.tri(x.s),], M1 != M2)

	# now plot just these values, adding labels (geom_text) for the names and th values

	p1 <- ggplot(data = mi.lower, aes(M1, M2, fill=corr)) + theme_bw() + geom_tile() + 
	geom_text(data=mi.lower, aes(label=paste(round(corr,3), pvalue))) +
	geom_text(data=mi.ids, aes(label=M2, colour="grey40"))

	# scale_colour_identity() will make the labels pick up the specified colours and the gradient for the scale_fill is specified
	meas <- as.character(unique(m$M2))
	p2 <- p1 + scale_colour_identity() +
	scale_fill_gradientn(colours= c("red", "white", "blue"), limits=c(1,-1)) +
	scale_x_discrete(limits=meas[length(meas):1]) + #flip the x axis
	scale_y_discrete(limits=meas)

	# add grouping
	if( !is.null(argList$group)) {
		mi.lower$groupVar = as.factor(argList$group)
		p2 <- p2 + facet_wrap(~argList$group, scale = 'free')
	}


	# Manage subtitle
	theTitle = paste('Correlations',subTitle,sep = '\n')

	p2 + xlab(NULL) + ylab(NULL) +
	opts(axis.text.x= theme_blank()) +
	opts(axis.text.y= theme_blank()) +
	opts(axis.ticks= theme_blank()) +
	opts(panel.border= theme_blank()) +
	opts(legend.position='none') +
	opts(title=theTitle)
}


corplot2 <- function(x,y) {
	#Make the correlation matrix
	(x.s <- cor(x,y))

	(x.s <- cor(EKO2,HRV2))
	# # same with p-values, then use symnum() to represent the values as asterisks
	cor.pval <- function(x,y, alternative="two-sided", ...) {

		corMat <- cor(x,y, ...) 
		n <- nrow(x) 
		df <- n - 2 
		STATISTIC <- sqrt(df) * corMat / sqrt(1 - corMat^2)     
		p <- pt(STATISTIC, df)
		p <- if (alternative == "less") {
			p 
		} 
		else if (alternative == "greater") {
			1 - p 
		}
		else 2 * pmin(p, 1 - p) 
		p 
	}
	p <- cor.pval(x,y)

	# symnum(x.s)

	# melt the correlation data
	m <- melt(x.s)

	stars <- as.character(symnum(p, cutpoints=c(0,0.001,0.01,0.05,1),
	symbols=c('***', '**', '*', '' ),
	legend=F))

	m$stars = stars

	#now put them alltogether (with melt() to reshape the correlation matrix) molten.iris
	names(m) <- c("M1", "M2", "corr", "pvalue")

	# # Define each triangle of the plot matric and the diagonal (mi.ids)
	# mi.ids <- subset(m, M1 == M2) # diagonal
	# mi.lower <- subset(m[lower.tri(x.s),], M1 != M2) # Upper
	# mi.upper <- subset(m[upper.tri(x.s),], M1 != M2)

	# now plot just these values, adding labels (geom_text) for the names and th values

	p1 <- ggplot(data = m, aes(M1, M2, fill=corr)) + theme_bw() + geom_tile() +
	geom_text(data = m, aes(label=paste(round(corr,3), pvalue)), colour = 'grey10')  # correlations as text

	# scale_colour_identity() will make the labels pick up the specified colours and the gradient for the scale_fill is specified
	meas <- as.character(unique(m$M2))
	p2 <- p1 + scale_colour_identity() +
	scale_fill_gradientn(colours= c("red", "white", "blue"), limits=c(1,-1)) +
	# scale_x_discrete(limits=meas[length(meas):1]) + #flip the x axis
	scale_y_discrete(limits=meas)

	p2 + xlab(NULL) + ylab(NULL) +
	opts(axis.ticks= theme_blank()) +
	opts(panel.border= theme_blank()) +
	opts(legend.position='none') +
	opts(title="Correlations")
}


