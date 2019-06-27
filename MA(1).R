# Package

install.packages("quantmod")
library(quantmod)

# Data

getSymbols('PBR',return.class = 'xts',index.class = 'Date',from = "2019-01-01",to = Sys.Date(),periodicity = "daily",src='yahoo')
data = PBR$PBR.Close # Petróleo Brasileiro S.A. - Petrobras (PBR) - NYSE (USD)

data = diff(data)[-1]

para = c(mean(data),0.2,var(data))

ma_1 = function(data,para){

e = matrix(NA,n,1)
n = dim(data)[1]
  
e[1] = 0
mu = para[1]
theta = para[2]
sigma2 = para[3]


loglik = -.5 * n * log(2 * pi)

for(i in 1:dim(data)[1]){
	e[i+1] = data[i]-mu-theta * e[i]
	e2 = (e[i+1])^2
	loglik = loglik - .5 * log(sigma2)-(e2/(2*sigma2))
	}
return(-loglik)
}

ma_1(data=data,para=para)

otim = optim(par=para,fn=ma_1,data=data,method="L-BFGS-B",lower = c(-Inf,-Inf,0),control=list("trace"=1))

results = as.matrix(otim$par)
rownames(results) = c("intercept","ma1","sigma^2")
results

# compare with arma {stats}

arima(data,order=c(0,0,1))$coef
