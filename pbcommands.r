pbcopy <- function(data, row.names = F){
	zz <- pipe('pbcopy','w'); write.table(data2$Age,file = zz, sep = '\t', row.names = row.names); close(zz)
	}