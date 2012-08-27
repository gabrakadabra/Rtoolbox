# Beräkna multvariat QQ-plot
# Fritt efter {Johnson:2007vn} s184-186

ChisqPlot <- function(data, abline = TRUE){
	
	# antal frihetsgrader
	p <- dim(data)[2]
	
	# antal observationer
	n <- dim(data)[1]
	
	# Mahalanobis d2-avstånd
	d2 <- mahalanobis(data, mean(data), cov(data))
	
	# Qvantiler
	probabilitylevels = (seq(1,n)-0.5)/n #q_{c,p}((i-.5)/n)
	chisq_quantiles = qchisq(probabilitylevels,p) #q_{c,p}

	x_label = paste('q_{c,',p,'}','((j-0.5)/',n,')',sep = '')
	plot(chisq_quantiles, sort(d2), main=paste('Chisq-plot',substitute(data),sep = ' '), xlab = x_label, ylab = substitute(data)) #EKO
	
	# lägg till en abline
	if(abline){
	abline(a = 0, b = 1, col = 'red')
	}
}
