# Author: Werley Cordeiro
# werleycordeiro@gmail.com

# Package

install.packages("quantmod")
library(quantmod)

# Data

getSymbols('PBR',return.class = 'xts',index.class = 'Date',from = "2019-01-01",to = Sys.Date(),periodicity = "daily",src='yahoo')
data = PBR$PBR.Close # Petróleo Brasileiro S.A. - Petrobras (PBR) - NYSE (USD)

# AR(p)
# OLS

p = 2

ar_p<-function(data,p){
T = dim(data)[1]
if(p==1){
	Y = as.numeric(data[-1])
	X = as.numeric(data[-T])
	X = cbind(rep(1,length(X)),X)
}else{
	if(p>1){
		X = matrix(NA,T-p,p)
			for(i in 1:p){
	 			if(i<p){
					 aux = as.numeric(data[1:(T-i)]) # exclude final observations according to order p
	 			  X[,i]= aux[-(1:(p-i))] # exclude initial observations according to order p
				}else{
					X[,i] = as.numeric(data[1:(T-p)]) # exclude only final observations in the last vector p
			}
		}
		Y = as.numeric(data[-(1:p)])
		X = cbind(rep(1,dim(X)[1]),X)
		}
}

b = solve(t(X) %*% X) %*% t(X) %*% Y
pars = expand.grid(orderp = 1:p)
names = paste0("Beta",pars$orderp)
rownames(b) = c("intercept",names)
return(b)
}
source("ar_p.R")
ar_p(data=data,p=p)

# Compare with ar{stats}
ar(data,FALSE,order.max=p,method="ols",demean=TRUE)
