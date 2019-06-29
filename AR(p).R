# Author: Werley Cordeiro
# werleycordeiro@gmail.com

# Package

install.packages("quantmod")
library(quantmod)

# Data

getSymbols('PBR',return.class = 'xts',index.class = 'Date',from = "2019-01-01",to = Sys.Date(),periodicity = "daily",src='yahoo')
data = PBR$PBR.Close # Petróleo Brasileiro S.A. - Petrobras (PBR) - NYSE (USD)

data = diff(data)[-1]

# Obs.: pacf(data) ?

# AR(p)

p = 1 # AR Order

source("ar_p.R")
ar_p(data=data,p=p,inter=TRUE,meth="mle")

# Compare with arima{stats}

ar(x=data,aic=FALSE,order.max=p,method="ols",demean=TRUE)
