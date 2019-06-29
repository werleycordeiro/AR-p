# AR(p)

ar_p<-function(data,p,inter,meth){

 source("dataf.R")
 dat = dataf(data=data,inter=inter,p=p)
 Y = dat$Y
 X = dat$X

 source("ols.R")
 b = ols(X=dat$X,Y=dat$Y)

 if(meth=="ols"){
   pars = expand.grid(orderp = 1:p)
   names = paste0("Beta",pars$orderp)
   if(inter){
      rownames(b) = c("intercept",names)
   }else{
      rownames(b) = c(names)}
      return(b)
 }else{
   if(meth=="mle"){
      para = matrix(NA,(p+2),1)
      para[1] = b[1,]
      para[2:(p+1)] = b[2:(p+1),]
      para[(p+2)] = var(data)

      ml = function(para,data,p,inter){

        T = dim(data)[1]
        data = data
        inter = inter
        p = p
 
        source("dataf.R")

        dat = dataf(data=data,inter=inter,p=p)
        Y = dat$Y
        X = dat$X
 
        beta = as.matrix(para[1:(p+1)])
        sigma2 = para[(p+2)]

        loglik = -.5 * (T-p) * log(2 * pi)

		    for(i in 1:(T-p)){
			      e = Y[i] - X[i,] %*% beta

			      loglik = loglik -.5 * log(sigma2) - ((e)^2)/(2*sigma2)
			      }
	          return(-loglik)
         }

         low = c(rep(-1,p+1),1e-100)

          otim<-optim(par=as.numeric(para),fn=ml,data=data,p=p,inter=inter,lower=low,method="L-BFGS-B",control=list("trace"=1))
          return(otim$par)
        }

   }
}
