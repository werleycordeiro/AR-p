p=1

para<- matrix(0.2,1,(p+2))
para[1]<-mean(data)
para[p+2] = as.numeric(var(data))


ml = function(para,data,p,inter){

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
		if(inter){X = cbind(rep(1,dim(X)[1]),X)}
		}
}


beta = as.matrix(para[1:(p+1)])
sigma2 = para[(p+2)]

loglik = -.5 * (T-p) * log(2 * pi)

		for(i in 1:(T-p)){
			e = Y[i] - X[i,] %*% beta

			loglik = loglik -.5 * log(sigma2) - ((e)^2)/(2*sigma2)
			}
	return(-loglik)
}




low = c(rep(-Inf,p+1),0)

otim<-optim(par=as.numeric(para),fn=ml,data=data,p=p,inter=inter,method="L-BFGS-B",lower=low,control=list("trace"=1))


