#Example of the computation of the price of American call on a stock that
#pays discrete dividends

# Need to have the package AmericanCallOpt installed
# The command install.packages("AmericanCallOpt") will install the package

library("AmericanCallOpt") #load the package

#--------------------------------------------------------------------------
# Binomial price of an American option with an underlying stock that
# pays proportional dividends in discrete t
S<-100                                       #stock price
K<-100                                       #strike price
r<-0.10                                      #interest rate
sigma<-0.25                                  #volatility
t<-1                                         #time to expiration
steps<-100                                   #number of time steps
dividend_times<-matrix( c(0.25, 0.75) )      #vector of ex-dividend dates
dividend_yields<-matrix( c(0.025, 0.025) )   #dividends as proportion of stock price
call_price_bin_propdiv<-am_call_bin_propdiv(S, K, r, sigma, t,
                                            steps, dividend_times, dividend_yields)
#---------------------------------------------------------------------------

