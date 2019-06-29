# OLS
# AR(p)

ar_p<-function(data,p,inter){

source("dataf.R")
dat = dataf(data=data,inter=inter,p=p)
Y = dat$Y
X = dat$X

b = solve(t(X) %*% X) %*% t(X) %*% Y
pars = expand.grid(orderp = 1:p)
names = paste0("Beta",pars$orderp)
if(inter){rownames(b) = c("intercept",names)}else{rownames(b) = c(names)}
return(b)
}
