library(TSA)
a=c(0.5,0.6);b=0.3
set.seed(1234567)
sim=garch.sim(alpha=a,beta=b,n=500)
plot(sim,type='l',ylab=expression(r[t]),xlab='t')
acf(sim)
pacf(sim)
acf(abs(sim))
pacf(abs(sim))
acf(sim^2)
pacf(sim^2)
g2=garch(sim,order=c(1,1))
summary(g2)
###------------------load the necessary functions and package------------
#TSA package 
library(TSA)
#likelihood function of GARCH(1,1) model with standard normal innovations.
source("TTL.R")
#My MLE procedure 
source("MyMLE.R")                            
#The comparison between "garch()" in TSA and my MLE procedure.
source("xitong.R")
# loading part END---------------------------------------------------------------------
##### 1 Monte Carlo. set the parameter values.
a=c(0.5,0.6);b=0.3
## 2 simulating time series, model fit and comparison.
set.seed(1234567)
source("duibi.R")
duibi(250,a,b)
duibi(500,a,b)
duibi(1000,a,b)
