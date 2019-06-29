ols = function(X,Y){

b = solve(t(X) %*% X) %*% t(X) %*% Y

return(b)

}