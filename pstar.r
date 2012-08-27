# Returnera en sträng med stjärnor
 # 0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
pstar <- function(x) {
	as.character(symnum( x , cutpoints=c(0,0.001,0.01,0.05,0.1,1), symbols=c('***', '**', '*', '.', '' ), legend=F))
}
