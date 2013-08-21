require(ggplot2)
require(reshape2)


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
	names(data_NA_m) <- c('X1','X2','X3')
	(p <- ggplot(data_NA_m, aes(x = as.factor(X1), y = X2)) + 
	   geom_tile(aes(fill = factor(X3)),colour = 'white'))+ 
# 	   scale_fill_manual('Is NA?',values = alpha(c("blue", "red"), 0.5))) +
# 	  ggtitle(label=paste('NA in:',opts(title = substitute(indata)))) + 
	  theme(axis.text.x= element_blank()) +
	  theme(axis.ticks= element_blank()) +
	  theme(panel.border= element_blank()) +
	  theme(legend.position='none') + 
	  theme_bw()
}
