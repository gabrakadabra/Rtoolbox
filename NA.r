require(ggplot2)

nna <- function(x, col = TRUE) {
	if (col) nNA <- colSums(is.na(x))	
	else nNA <- rowSums(is.na(x))

	tmo2 <- data.frame(nNA)
	tmo2$nAviable <- (dim(x)[1] - tmo2)[,]
	print(tmo2)
	return(nNA)
}

plotNA <-  function(indata) {
	data_NA <- is.na(indata)
	data_NA_m <- melt(data_NA)
	(p <- ggplot(data_NA_m, aes(as.factor(X1), X2)) + geom_tile(aes(fill = factor(value)),colour = 'white') + scale_fill_manual('Is NA?',values = alpha(c("blue", "red"), 0.5))) +
	opts(title = paste('NA in:',opts(title = substitute(indata)))) + 
	opts(axis.text.x= theme_blank()) +
	opts(axis.ticks= theme_blank()) +
	opts(panel.border= theme_blank()) +
	opts(legend.position='none') + 
	theme_bw()
}
