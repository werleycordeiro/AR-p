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
      if(inter){
      para = matrix(NA,(p+2),1)
      para[1] = b[1,]
      para[2:(p+1)] = b[2:(p+1),]
      para[(p+2)] = var(data)
      low = c(rep(-Inf,p+1),1e-100)
      }else{
         para = matrix(NA,(p+1),1)
         para[1:p] = b[(1:p),]
         para[(p+1)] = var(data)
         low = c(rep(-Inf,p),1e-100)
         }
       source("mle.R")
       
       otim<-optim(par=as.numeric(para),fn=mle,data=data,p=p,inter=inter,lower=low,method="L-BFGS-B",control=list("trace"=1))
       return(otim$par)
        }

   }
}
