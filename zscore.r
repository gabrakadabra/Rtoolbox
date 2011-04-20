# Z-score för data frame
zscore <- function(x) {

	stdfun <- function(inp){
		out <- (inp - mean(inp)) / sqrt( var(inp) )
	}

	if (is.data.frame(x)){
		y <- apply(x,2, stdfun )
	}
	else {
		y <- stdfun(x)
	}

	return(y)
}