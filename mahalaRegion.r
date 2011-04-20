mahalaRegion <- function(data,alpha){

	data_mahala <- mahalanobis(data,colMeans(data),cov(data))

	# Beräkna en alpha-ellips enligt Johnsson/Wicherns
	dim <- dim(data)
	Ftest <- (dim[2])*((dim[1] - 1)/(dim[1]- dim[2])) * qf(alpha,dim[2],dim[1] - dim[2],lower.tail = F) #[p(n-1)/(n-p)] F_{p,n-p}[\alpha]
	mahala_region <- data_mahala > Ftest
	
	mahalaOut <- data.frame(mahala_region, data_mahala)
	names(mahalaOut) <- c('region','mahala')
	
	return(mahalaOut)
}