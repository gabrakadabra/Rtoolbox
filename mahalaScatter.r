# require(lattice)
# 
# pl <- splom(EKO2, auto.key = list(columns = 2, title = "Outlier"))
# quartz()
# print(pl)
# 
# mahalaEKO2 = mahalanobis(EKO2,mean(EKO2),cov(EKO2))
# mahalaPLUS95 <- mahalaEKO2 < qchisq(.05,dim(EKO2)[2],lower.tail = F)
# # splom(~USArrests | state.region, pscales = 0)
# quartz()
# splom(~EKO2, pscales = 0, type = c('g','p'), group = mahalaPLUS95)
# 
# 
# names(EKO2)
