dataf = function(data,inter,p){
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
return(list(X=X,Y=Y))
}

