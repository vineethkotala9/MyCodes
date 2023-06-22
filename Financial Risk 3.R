
# Two useful libraries:
library(xts)
library(rmgarch)

#read the csv file containing the ETF simple returns and compute log returns
ETFreturns <- read.csv("ETFreturns.csv")

#compute log returns for most recent 1,000 days for the 4 ETFs
returns <- data.frame(date = ETFreturns[(nrow(ETFreturns)-999):nrow(ETFreturns),1],
                         SPY = log(1+ETFreturns[(nrow(ETFreturns)-999):nrow(ETFreturns),2]),
                         XLB = log(1+ETFreturns[(nrow(ETFreturns)-999):nrow(ETFreturns),3]),
                         XLE = log(1+ETFreturns[(nrow(ETFreturns)-999):nrow(ETFreturns),5]),
                         XLF = log(1+ETFreturns[(nrow(ETFreturns)-999):nrow(ETFreturns),6])
                         )

## Question 1
## Note: The function dccfit() will be used; it estimates the DCC model in two stages
## First a univariate GARCH model is fitted to each return series
## The standardized residuals are then extracted
## Then a model with dynamically changing conditional correlation matrix is fitted

## As an initial step (not required), estimate a univariate GARCH model for each return process
## Specify univariate GARCH(1,1) model and set mean return = 0
uspec <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,1)),
                    mean.model = list(armaOrder = c(0,0), include.mean = FALSE),
                    distribution.model = "norm") 

## Check the univariate specification for the six series
fit.marg1 <- ugarchfit(spec = uspec, data = returns[,2])
fit.marg2 <- ugarchfit(spec = uspec, data = returns[,3])
fit.marg3 <- ugarchfit(spec = uspec, data = returns[,4])
fit.marg4 <- ugarchfit(spec = uspec, data = returns[,5])
coef(fit.marg1)
coef(fit.marg2)
coef(fit.marg3)
coef(fit.marg4)


## Now turn to estimating the DCC model
## Combine univariate specifications of the 4 GARCH models
marginspec <- multispec(replicate(4, uspec))

## Create DCC(1,1) specification
mspec <- dccspec(marginspec, dccOrder = c(1,1), model = "DCC", distribution = "mvnorm")

## Fit the DCC(1,1) model
mod <- dccfit(mspec,returns[,2:5])
mod
coef(mod)