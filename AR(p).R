# Author: Werley Cordeiro
# werleycordeiro@gmail.com

# Package

install.packages("quantmod")
suppressMessages(library(quantmod))

# Data

getSymbols('PBR',return.class = 'xts',index.class = 'Date',from = "2019-01-01",to = Sys.Date(),periodicity = "daily",src='yahoo')
data = PBR$PBR.Close # Petróleo Brasileiro S.A. - Petrobras (PBR) - NYSE (USD)

data = diff(data)[-1]

# Obs.: pacf(data) ?

# AR(p)

p = 2 # AR Order
inter = TRUE
source("ar_p.R")
ar_p(data=data,p=p,inter=inter,meth="mle") # inter: intercept = TRUE or FALSE. meth  = "ols" or "mle"

# Compare with ar{stats}

ar(x=data,aic=FALSE,order.max=p,method="ols",demean=inter)
arima(data,order=c(p,0,0),method="ML")
