mle = function(para,data,p,inter){
  
  T = dim(data)[1]
  
  source("dataf.R")
  
  dat = dataf(data=data,inter=inter,p=p)
  Y = dat$Y
  X = dat$X
  
  if(inter){
    beta = as.matrix(para[1:(p+1)])
    sigma2 = para[(p+2)]
  }else{
    beta = as.matrix(para[1:p])
    sigma2 = para[(p+1)]
  }
  
  loglik = -.5 * (T-p) * log(2 * pi)
  
  for(i in 1:(T-p)){
    e = Y[i] - as.matrix(X)[i,] %*% beta
    
    loglik = loglik -.5 * log(sigma2) - ((e)^2)/(2*sigma2)
  }
  return(-loglik)
}