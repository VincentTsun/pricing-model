library("derivmkts")

#the earliest autocallable event is at tt=1

#closing stock prices on oct 13 2020
AQN_s_0 = 20.47
BIP_s_0 = 61.76
BEP_s_0 = 47.09
NPI_s_0 = 42.58

#number of shares
AQN_n = (20000000*0.25)/AQN_s_0
BIP_n = (20000000*0.25)/BIP_s_0
BEP_n = (20000000*0.25)/BEP_s_0
NPI_n = (20000000*0.25)/NPI_s_0

#historical volatility (need to calculate using excel)
AQN_v = 0.302948
BIP_v = 0.260282
BEP_v = 0.204378
NPI_v = 0.223673

#dividends based on trailing dividends (yahoo finance)
AQN_div = 0.0375
BIP_div = 0.028
BEP_div = 0.0268
NPI_div = 0.0317

r = 0.0017 #1 year treasury bill yield as risk free rate at oct 13 2020
h = 0.5 #a constant value for semi annual periods


nSimul = 10000000 #number of simulations
price_array <- c() #create array to store simulation results

for(i in 1:nSimul){ #loop for number of simulations
  #reset per period initial stock price 
  AQN_s_t = AQN_s_0 
  BIP_s_t = BIP_s_0
  BEP_s_t = BEP_s_0
  NPI_s_t = NPI_s_0
  tt = 0 #reset tt
  pv_payoff = 0 #reset cumulative payoff
  for(semi_yr in 0:14){ #repeat for 14 period (2 periods a year for 7 years)
    
    if(tt<7){ #if tt is less than 7 years 
      tt = tt + 0.5 #increase tt by 0.5 each loop
      Z_path = rnorm(4, mean=0, sd=1) #normal distribution
      #forecasted stock prices, substituted the tt to 0.5(half year period)
      AQN_s_t_1 = AQN_s_t*exp((r-AQN_div-0.5*AQN_v^2)*h + AQN_v*sqrt(h)*Z_path[1])
      BIP_s_t_1 = BIP_s_t*exp((r-BIP_div-0.5*BIP_v^2)*h + BIP_v*sqrt(h)*Z_path[2])
      BEP_s_t_1 = BEP_s_t*exp((r-BEP_div-0.5*BEP_v^2)*h + BEP_v*sqrt(h)*Z_path[3])
      NPI_s_t_1 = NPI_s_t*exp((r-NPI_div-0.5*NPI_v^2)*h + NPI_v*sqrt(h)*Z_path[4])
    
      portfolio_performance = (AQN_s_t_1*AQN_n+BIP_s_t_1*BIP_n+BEP_s_t_1*BEP_n+NPI_s_t_1*NPI_n)/20000000
      if(portfolio_performance >= 0.7){ #if portfolio performance is above 70%, coupon payment occurs
        pv_payoff = pv_payoff + 3.5*exp(-r*tt)
      }
      if(portfolio_performance >= 1){ #if portfolio performance is above 100%, return 100% principle
        if(tt>0.5){ #adding another if function to limit autocall to after the first observation date
          pv_payoff = pv_payoff + 100*exp(-r*tt)
          tt = 8 #set tt to above 7 to prevent coupon calculation in the future
        }
      }  
      if(tt == 7){  #if reached maturity,
        if(portfolio_performance < 0.7){ #return principle based on performance if performance is below 70%
          pv_payoff = pv_payoff + portfolio_performance*100*exp(-r*tt)
        }
        else{ #return 100% principle if above 70%
          pv_payoff = pv_payoff + 100*exp(-r*tt)
        }
       
      }
      #set the initial stock prices of the new period to the previous end stock prices
      AQN_s_t = AQN_s_t_1
      BIP_s_t = BIP_s_t_1
      BEP_s_t = BEP_s_t_1
      NPI_s_t = NPI_s_t_1
      
    }
    else { 
      #if autocalled or reached maturity, add cumulative payoff to the array 
      #and break the loop, starting another simulation
      price_array[i] = pv_payoff
      break
    }
  }
}

hist(price_array, breaks = 30, col = "red") #histogram
price = mean(price_array) #average simulation results
price 

