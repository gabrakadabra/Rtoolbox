# o2pls
# Enligt {Trygg:2002kj}

source('./zscore.r')

o2pls <- function(X,Y) {

	# # 1)
	# K <- solve( t(Y)%*%Y ) %*% t(Y) %*% t(X)
	# 
	# # 2)
	# K <- W %*% t(Kp) + KE
	# 
	# # 3) Beräkna T
	# T = <- X %*% W %*% solve( t(W)%*%W )
	# 
	# # 10)
	# C  <- t( solve( t(T)%*%T ) %*% t(T) %*% Y )
	# Yhat <- T %*% t(C)
	# 
	# 
	# 
	# Tp
	# To
	# Wp
	# Wo
	# E
	# 
	# Up
	# Uo
	# Cp
	# Co
	# F
	
	Y <- data2.EKO
	X <- data2.HRV
	
	Y <- as.matrix(Y)
	X <- as.matrix(X)

	Y <- zscore(Y)
	X <- zscore(X)
	
	# Beräkna kovariansmatrisen
	S <- t(Y) %*% X
	
	# 1)
	# SVD på kovariansmatrisen
	S.svd <- svd(S)
	
	C <- S.svd$u		#Egenvärden för X'YY'X
	D <- diag(S.svd$d)
	W <- S.svd$v		#Egenvärden för Y'XX'Y
	
	# 2)
	# X score matrix T
	T <- X %*% W
	
	# 3)
	EXY <- Y - U %*% t(C)
	wYosc <- PCA( t(EXY) %*% T)
	
	
	# Update scorematrix
	T <- X %*% W
	
	# 4)
	U <- Y %*% C
	
	# 5)
	cXosc <- PCA( t(FXY) %*% U)
	
	# Predict U
	BU <- solve( t(U)%*%U ) %*% t(U) %*% T
	# Predict T
	BT <- solve( t(T)%*%T ) %*% t(T) %*% U
}