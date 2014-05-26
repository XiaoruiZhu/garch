#---------------数据下载载入包---------
library(tseries)
library(xts)
library(fBasics)
source("getdata.R")

#---------------挑选各个股指-----------
#   9I0001  ^DJI  Dow Jones Averages(30 Industrials)	道琼斯30指数

FTSE
########### OK ###########
FTSE=getdata(quote="^FTSE",start="2000-01-02",end="2014-03-31",savename="FTSE")
FTSE[1:3,]
length(FTSE[,1])
par(mfrow = c(2, 3)) 
plot(FTSE[,6],type='l',xlab="时间",ylab="FTSE")
FTSE=read.csv('Y:\\DATA\\FTSE2000-01-02_2014-03-31.csv',header=TRUE,sep=',')
FTSE[1:10,7]
length(FTSE[,7])
plot(FTSE[,7],type='l',xlab="时间",ylab="FTSE")
return_FTSE=diff(FTSE[,7])/FTSE[,7][-(length(FTSE[,7]))]
return_FTSE[1:10]
hFTSE=length(return_FTSE)
hFTSE
kurtosis(return_FTSE)
mean(return_FTSE)
sd(return_FTSE)
# Augmented Dickey-Fuller test
adf.test(return_FTSE)
plot(return_FTSE,type='l',xlab="时间",ylab="Return_FTSE")
para0FTSE=xitong(hFTSE,return_FTSE)$coef
para0FTSE
para1FTSE=MyMLE(hFTSE,return_FTSE)$mle.N
para1FTSE
library(fGarch)
para2FTSE=garchFit(formula=~garch(1,1),data=return_FTSE)
list(para2FTSE)
MytQMLE(hFTSE,return_FTSE,dfest=10)
para3_FTSE=A_tQMLE(h=hFTSE,x=return_FTSE)
para3_FTSE
kurtosis(rt(3500,7))
# S&P 500 代码是"^GSPC"  这个用来模拟真实的论文
sp500=getdata(quote="^GSPC",start="2000-01-02",end="2014-03-31",savename="S&P")
length(sp500[,1])
sp500[1,]
plot(sp500[,6],type='l',xlab="时间",ylab="S&P 500")
SP500=read.csv('Y:\\DATA\\S&P2000-01-02_2014-03-31.csv',header=TRUE,sep=',')
# 计算回报率return
plot(SP500[,7],type='l',xlab="时间",ylab="S&P 500")
# SP500[1:10,7]
# length(diff(SP500[,7],lag=10))
# -length(SP500[,7])
# SP500[1:20,7]
# SP500[1:20,7][-10]
# length(diff(SP500[1:30,7],lag=10))
# c((length(SP500[,7])-9):(length(SP500[,7])))
return_SP=diff(SP500[,7])/SP500[,7][-(length(SP500[,7]))]
return_SP[3460]
hSP=length(return_SP)
hSP
kurtosis(return_SP)
summary(return_SP)
sd(return_SP)
# Augmented Dickey-Fuller test
adf.test(return_SP)

plot(return_SP,type='l',xlab="时间",ylab="Return_S&P500")
para0=xitong(hSP,return_SP[1:hSP])$coef
para0
library(BB)
library(fGarch)
para1=MyMLE(hSP,return_SP)$mle.N
para1
para2=garchFit(formula=~garch(1,1),data=return_SP)
list(para2)
MytQMLE(hSP,return_SP,dfest=10)
para3_SP500=A_tQMLE(h=hSP,x=return_SP)
para3_SP500

#   纳斯达克综合指数 "NDAQ"
########### OK ###########
ndaq=getdata(quote="^IXIC",start="2000-01-02",end="2014-03-31",savename="NDAQ")
ndaq[1]
plot(NDAQ[,7],type='l',xlab="时间",ylab="NASDAQ")
NDAQ=read.csv('Y:\\DATA\\NDAQ2000-01-02_2014-03-31.csv',header=TRUE,sep=',')
NDAQ[1:10,7]
mean(return_NDAQ)
sd(return_NDAQ)
kurtosis(return_NDAQ)
length(NDAQ[,7])
plot(NDAQ[,7],type='l',xlab="时间",ylab="NASDAQ")
return_NDAQ=diff(NDAQ[,7])/NDAQ[,7][-(length(NDAQ[,7]))]
return_NDAQ[1:10]
hND=length(return_NDAQ)
# Augmented Dickey-Fuller test
adf.test(return_NDAQ)
plot(return_NDAQ,type='l',xlab="时间",ylab="Return_NASDAQ")

para0NDAQ=xitong(hND,return_NDAQ)$coef
para0NDAQ
para1NDAQ=MyMLE(hND,return_NDAQ)$mle.N
para1NDAQ
para2NDAQ=garchFit(formula=~garch(1,1),data=return_NDAQ)
list(para2NDAQ)
MytQMLE(hND,return_NDAQ,dfest=100)
para3_NDAQ=A_tQMLE(h=hND,x=return_NDAQ)
para3_NDAQ

#   德国DAX ^GDAXI
# test111=get.hist.quote("^GDAXI","2000-01-02","2013-07-31",
#                provider ="yahoo",
#                quote=c("Open", "High", "Low", "Close","Volume","AdjClose"))
# test111[1:3,]
########### OK ###########
GDAXI=getdata(quote="^GDAXI",start="2000-01-02",end="2014-03-31",savename="GDAXI")
GDAXI[1,]
plot(GDAXI[,6],type='l',xlab="时间",ylab="DAX")
GDAXI=read.csv('Y:\\DATA\\GDAXI2000-01-02_2014-03-31.csv',header=TRUE,sep=',')
GDAXI[1:10,7]
length(GDAXI[,7])
plot(GDAXI[,7],type='l',xlab="时间",ylab="DAX")
# return_GDAXI=diff(GDAXI[,7],lag=10)/GDAXI[,7][-c((length(GDAXI[,7])-9):(length(GDAXI[,7])))]
return_GDAXI=diff(GDAXI[,7])/GDAXI[,7][-(length(GDAXI[,7]))]
return_GDAXI[1:10]
hGDAXI=length(return_GDAXI)
kurtosis(return_GDAXI)
summary(return_GDAXI)
sd(return_GDAXI)
plot(return_GDAXI,type='l',xlab="时间",ylab="Return_DAX")
# Augmented Dickey-Fuller test
adf.test(return_GDAXI)
para0GDAXI=xitong(hGDAXI,return_GDAXI)$coef
para0GDAXI
para1GDAXI=MyMLE(hGDAXI,return_GDAXI)$mle.N
para1GDAXI
para2GDAXI=garchFit(formula=~garch(1,1),data=return_GDAXI)
list(para2GDAXI)
MytQMLE(hGDAXI,return_GDAXI,dfest=100)
para3_GDAXI=A_tQMLE(h=hGDAXI,x=return_GDAXI)
para3_GDAXI

#   法国巴黎CAC "
FCHI=getdata(quote="^FCHI",start="2000-01-02",end="2014-03-31",savename="FCHI")
FCHI[1,]
plot(FCHI[,6],type='l',xlab="时间",ylab="CAC")
FCHI=read.csv('Y:\\DATA\\FCHI2000-01-02_2014-03-31.csv',header=TRUE,sep=',')
FCHI[1:10,7]
length(FCHI[,7])
plot(FCHI[,7],type='l',xlab="时间",ylab="CAC")
# return_FCHI=diff(FCHI[,7],lag=10)/FCHI[,7][-c((length(FCHI[,7])-9):(length(FCHI[,7])))]
return_FCHI=diff(FCHI[,7])/FCHI[,7][-c(length(FCHI[,7]))]

return_FCHI[1:10]
hFCHI=length(return_FCHI)
hFCHI
kurtosis(return_FCHI)
mean(return_FCHI)
sd(return_FCHI)
# Augmented Dickey-Fuller test
adf.test(return_FCHI)
plot(return_FCHI,type='l',xlab="时间",ylab="Return_CAC")
# Augmented Dickey-Fuller test
adf.test(return_NDAQ)
para0FCHI=xitong(hFCHI,return_FCHI)$coef
para0FCHI
para1FCHI=MyMLE(hFCHI,return_FCHI)$mle.N
para1FCHI
para2FCHI=garchFit(formula=~garch(1,1),data=return_FCHI)

MytQMLE(hFCHI,return_FCHI,dfest=56)
para3_FCHI=A_tQMLE(h=hFCHI,x=return_FCHI)
para3_FCHI

#   香港恒生指数 ^HSI
HSI=getdata(quote="^HSI",start="2000-01-02",end="2014-03-31",savename="HSI")
HSI[1,]
plot(HSI[,6],type='l',xlab="时间",ylab="香港恒生指数")
HSI=read.csv('Y:\\DATA\\HSI2000-01-02_2014-03-31.csv',header=TRUE,sep=',')
HSI[1:10,7]
length(HSI[,7])
plot(HSI[,7],type='l',xlab="时间",ylab="香港恒生指数")
return_HSI=diff(HSI[,7],lag=10)/HSI[,7][-c((length(HSI[,7])-9):(length(HSI[,7])))]
return_HSI=diff(HSI[,7])/HSI[,7][-c(length(HSI[,7]))]
return_HSI[1:10]
hHSI=length(return_HSI)
hHSI
kurtosis(return_HSI)
mean(return_HSI)
sd(return_HSI)
# Augmented Dickey-Fuller test
adf.test(return_HSI)
plot(return_HSI,type='l',xlab="时间",ylab="Return_HSI")
para0HSI=xitong(hHSI,return_HSI)$coef
para0HSI
para1HSI=MyMLE(hHSI,return_HSI)$mle.N
para1HSI
para2HSI=garchFit(formula=~garch(1,1),data=return_HSI)

MytQMLE(hHSI,return_HSI,dfest=22)
para3_HSI=A_tQMLE(h=hHSI,x=return_HSI)
para3_HSI

#   欧洲道琼斯50指数 "SX5P:IND"
# sx5p=getdata(quote="SX5P",start="2002-01-02",savename="SX5P")
# sx5p[1]

#   中石油"601857.ss"
# zsy=getdata(quote="601857.ss",start="2013-01-01",savename="601857")
# zsy

con <- url("http://quote.yahoo.com")
if(!inherits(try(open(con), silent = TRUE), "try-error")) {
  close(con)
  x <- get.hist.quote(instrument = "^DJI", start = "1998-01-01",
                      quote = "Close")
  plot(x)
}
if(!inherits(try(open(con), silent = TRUE), "try-error")) {  
  x <- get.hist.quote(instrument = "ibm", quote = c("Cl", "Vol"))
  plot(x, main = "International Business Machines Corp")
  
  spc <- get.hist.quote(instrument = "^gspc", start = "1998-01-01",
                        quote = "Close")
  ibm <- get.hist.quote(instrument = "ibm",  start = "1998-01-01",
                        quote = "AdjClose")
  require("zoo")  	# For merge() method.
  x <- merge(spc, ibm)
  plot(x, main = "IBM vs S&P 500")
}
#### diff(a)/a[-length(a)]
