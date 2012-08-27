#-----------------------------------------------------------------------------------------------------------------------------------------------
# Beroenden
require(ggplot2)

# Ålderskorringering av data
agecor <- function(data, age, center_age, plot = FALSE, ... ){	
	argList<-list(...)
	
	if( !is.null(argList$subSet)) {
		subSet = argList$subSet
	} else {
		subSet = TRUE
	}
	
	if(is.data.frame(data)){
		corrected_data <- data.frame()
		for (i in dim(data)[2]) {
			linear_model = lm(data[subSet,i]~age[subSet])
			c_data = data - linear_model$coeff[2]*(age-center_age)
			corrected_data = c_data
		}
	} 
	else {
		linear_model = lm(data[subSet]~age[subSet])
		corrected_data = data - linear_model$coeff[2]*(age-center_age)
	}

	#Plotting
	data2  <- data.frame(data,corrected_data)
	data2$age <- age

	if(plot){
		quartz()
		k <- ggplot(data = data2, aes(x= age)) 
		k <- k + 
		geom_point(aes(x = age, y = data, colour = 'Not corrected')) + 
		geom_smooth(aes(y = data, colour = 'Not corrected'),method="lm", se = F) + 
		geom_point(aes(y = corrected_data, colour = 'Corrected')) + 
		geom_smooth(aes(y = corrected_data , colour = 'Corrected'), method="lm", se = F) + 
		xlab(substitute(age)) + 
		ylab(substitute(data)) + 
		opts(title = 'Age correction') + labs(colour="Corrected/Not corrected")
		print(k)
	}

	return(corrected_data)
}
# #-----------------------------------------------------------------------------------------------------------------------------------------------
# # Dokumentering
# 
# # exemple 1)
# 
# # create a new function 
# agecorFAP <- function(data){
# 	agecor(data, FAPdata$AGE, mean(FAPdata$AGE), plot = TRUE) #plotta korrigering = TRUE
# }
# 
# # use sapply on dataframes
# FAP_HRV <- as.data.frame(sapply(FAP_HRV,agecorFAP))
# names(FAP_HRV) <- c('VLF','LF','HF')
