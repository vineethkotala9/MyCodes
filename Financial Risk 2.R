#read the csv file containing the ETF simple returns and compute log returns
#GHW2 requires only the SPY returns; but compute returns on XLB, XLE, and XLF for GHW3
ETFreturns <- read.csv("ETFreturns.csv")

ETFlogreturns <- data.frame(date = ETFreturns[1:nrow(ETFreturns),1],
                         SPY = log(1+ETFreturns[1:nrow(ETFreturns),2]),
                         XLB = log(1+ETFreturns[1:nrow(ETFreturns),3]),
                         XLE = log(1+ETFreturns[1:nrow(ETFreturns),5]),
                         XLF = log(1+ETFreturns[1:nrow(ETFreturns),6])
                         )

#select the most recent 1,000 returns
ret <- ETFlogreturns[(nrow(ETFlogreturns)-999):nrow(ETFlogreturns),2]

#sample variance and  standard deviation of log returns
variance = sum(ret^2)/length(ret)
stddev = sqrt(sum(ret^2)/length(ret))

##Q1a
# parameters to estimate are alpha, beta, sigma, sigma_1

#Objective function (negative log likelihood)
garch11a <- function(x) {  
  sigmasqhat = rep(0.0,length(ret))
  sigmasqhat[1] = x[4]^2 
  
  #(negative) log likelihood
  if (x[1]+x[2]>=1 || x[1]<0 || x[2]<0 || x[3]<0.001||  x[4]<0.001){
    NeglogLH = 9999
  } else {
    for (i in 1:(length(ret)-1)) {
      sigmasqhat[i+1] = (1-x[1]-x[2])*x[3]^2+x[1]*ret[i]^2+x[2]*sigmasqhat[i]
    }
    f <-(1/(sqrt(2*pi*sigmasqhat)))*exp(-0.5*ret^2/sigmasqhat)
    NeglogLH = -sum(log(f))
  }
  return(NeglogLH)
}
guess = c(0.1,0.8,0.01,0.01) #initial values
Q1a = optim(guess,garch11a)
alphaQ1a = Q1a$par[1]
betaQ1a = Q1a$par[2]
sigmaQ1a = Q1a$par[3]
sigma1Q1a = Q1a$par[4]



##Q1b
# parameters to estimate are alpha and beta
# sigma and sigma_1 are set to sample std. deviation

#(negative) log likelihood
garch11b <- function(x) {
  sigmasqhat = rep(0,length(ret))
  sigmasqhat[1] = stddev^2  
  
  #(negative) log likelihood
  if (x[1]+x[2]>=1 || x[1]<0 || x[2]<0 ){
    NeglogLH = 9999
  } else {
    for (i in 1:(length(ret)-1)) {
      sigmasqhat[i+1] = (1-x[1]-x[2])*stddev^2 + x[1]*ret[i]^2 + x[2]*sigmasqhat[i]
    }
    
    #Likelihood 
    f <-(1/(sqrt(2*pi*sigmasqhat)))*exp(-0.5*ret^2/sigmasqhat)
    NeglogLH = -sum(log(f))
  }
  
  return(NeglogLH)
}

# Optimization #
Q1b <- optim(c(0.1,0.8),garch11b)
alphaQ1b = Q1b$par[1]
betaQ1b = Q1b$par[2]


##Question 2a
#use the estimates from Question 1a to compute conditional variance for January 3, 2023
#this is the day after the last date of data
sigmasqhat = rep(0,length(ret)+1)
sigmasqhat[1] = sigma1Q1a^2 
for (i in 1:length(ret)) {
  sigmasqhat[i+1]=(1-Q1a$par[1]-Q1a$par[2])*Q1a$par[3]^2+Q1a$par[1]*ret[i]^2+Q1a$par[2]*sigmasqhat[i]
}
Q2a1 = sigmasqhat[length(ret)+1]
Q2a2 = sqrt(sigmasqhat[length(ret)+1])

##Question 2b
#let's write a function to compute the garch11 variance forecasts
#arguments are alpha, beta, sigma, return at t, cond. variance at t, x[6] = K (days)
garchforecast <- function(x) {  
  K = x[6]
  sigmasqhat = rep(0,K)
  sigmasqhat[1] = (1-x[1]-x[2])*x[3]^2+x[1]*x[4]^2 + x[2]*x[5]
  
  #now compute the variance forecasts through day K
  for (i in 2:K) {
    sigmasqhat[i] = x[3]^2 + (x[1]+x[2])^(K-1) * (sigmasqhat[1]-x[3]^2)
  }
  forecast = sum(sigmasqhat)
  return(forecast)
}

input = c(Q1a$par[1],Q1a$par[2],Q1a$par[3],ret[length(ret)],sigmasqhat[length(ret)],21)
Q2b = garchforecast(input)
Q2c = sqrt((252/21)*Q2b)

#Question 3
#Q3a
#install.packages("tseries") #must have installed package "tseries"
library(tseries)

inter = (1-Q1a$par[1]-Q1a$par[2])*Q1a$par[3]^2
Q3a = garch(ts(ret),order = c(1,1),control = garch.control(start = c(inter,Q1a$par[1],Q1a$par[2])))
# Q3a= garch(ts(ret),order = c(1,1)) #almost same as garchfit solution
omega_Q3a <- coef(Q3a)[1]
alpha_Q3a <- coef(Q3a)[2]
beta_Q3a <- coef(Q3a)[3]
sigmasq_Q3a <- omega_Q3a/(1-alpha_Q3a-beta_Q3a)

#Q3b
#install.packages("fGarch")
library(fGarch)

Q3b = garchFit( ~ garch(1,1), data = ret, include.mean = FALSE)
#Q3b = garchFit( ~ arma(1,0)+garch(1,1), data = ret, include.mean = TRUE)

omega_Q3b <- coef(Q3b)[1]
alpha_Q3b <- coef(Q3b)[2]
beta_Q3b <- coef(Q3b)[3]
sigmasq_Q3b <- omega_Q3b/(1-alpha_Q3b -beta_Q3b)


#Question 4 NGARCH
# parameters to estimate are alpha, beta, sigma, sigma_1, theta
#Q4a
ngarch11 <- function(x) {
  #x[1] = alpha
  #x[2] = beta
  #x[5] = theta
  sigmasqhat = rep(0,length(ret))
  sigmasqhat[1] = x[4]^2 
  if (x[1]*(1+x[5]^2)+x[2]>=1 || x[1]<0 || x[2]<0 || x[3]<0.001||  x[4]<0.001){
    NeglogLH = 9999
  } else {
    for (i in 1:(length(ret)-1)) {
      sigmasqhat[i+1] = (1-x[1]*(1+x[5]^2)-x[2])*x[3]^2+x[1]*(ret[i]-x[5]*sqrt(sigmasqhat[i]))^2+x[2]*sigmasqhat[i]
    }
    f <-(1/(sqrt(2*pi*sigmasqhat)))*exp(-0.5*ret^2/sigmasqhat)
    NeglogLH = -sum(log(f))
  }
  return(NeglogLH)
}
guess = c(0.1,0.8,0.03,0.04,0.5)
Q4 = optim(guess,ngarch11)
alphaQ4 = Q4$par[1]
betaQ4 = Q4$par[2]
sigmaQ4 = Q4$par[3]
sigma1Q4 = Q4$par[4]
thetaQ4 = Q4$par[5]

#Q4b
#Model 0 is a special case of model 1
#LR = 2(ln(L1 ) ??? ln(L0))
NEGL1 <- Q4$value 
NEGL0 <- Q1a$value
LRQ4 <- -2*(NEGL1 - NEGL0)
pvalQ4 <- pchisq(LR, df = 1, lower.tail = FALSE) #less than 0.01 so can reject the null hypothesis.


#Question 5 ARCH(1)
#parameters to estimate alpha, sigma, sigma_1

#Estimate ARCH(1)
arch1 <- function(x) {  
  sigmasqhat = rep(0,length(ret))
  sigmasqhat[1] = x[3]^2 
  
  #(negative) log likelihood
  if (x[1] >= 1 || x[1]<0 ||  x[2]<0.001||  x[3]<0.001){
    NeglogLH = 9999
  } else {
    for (i in 1:(length(ret)-1)) {
      sigmasqhat[i+1] = (1-x[1])*x[2]^2+x[1]*ret[i]^2
    }
    f <-(1/(sqrt(2*pi*sigmasqhat)))*exp(-0.5*ret^2/sigmasqhat)
    NeglogLH = -sum(log(f))
  }
  return(NeglogLH)
}
guess = c(0.1,0.01,0.01) #initial values
Q5 = optim(guess,arch1)
alphaQ5 = Q5$par[1]
sigmaQ5 = Q5$par[2]
sigma1Q5 = Q5$par[3]


#LR Test
NEGL1 <- Q1a$value
NEGL0 <- Q5$value #this is the special case
LRQ5 <- -2*(NEGL1 - NEGL0)
pvalQ5 <- pchisq(LR, df = 1, lower.tail = FALSE) #less than 0.01 so can reject the null hypothesis.



#instead we can use fGarch
Q5b <- garchFit(~garch(1,0), data = ret, include.mean = FALSE )
NEGL0Q5b <- getElement(Q5b@fit$llh, "LogLikelihood")
NEGL1Q3b <- getElement(Q3b@fit$llh, "LogLikelihood")
LR <- -2*(NEGL1Q3b - NEGL0Q5b)
pval <- pchisq(LR, df = 1, lower.tail = FALSE) #less than 0.01 so can reject the null hypothesis.



