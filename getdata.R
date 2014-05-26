#-------------抓取股票数据-------
#JPM是股指名称，自己上yahoo查
# JPM <- as.xts(get.hist.quote("JPM",start="2000-01-02",
#                              provider ="yahoo",
#                              quote=c("Open", "High", "Low", "Close","Volume","AdjClose")))
# write.csv(JPM,file="Y:/DATA/JPM2013.csv")
# length(JPM)

getdata=function(quote,start,end,savename){
  quote=get.hist.quote(quote,start=start,end=end,
                            provider ="yahoo",
                            quote=c("Open", "High", "Low", "Close","Volume","AdjClose"))
  save=paste("Y:/DATA/",savename,start,"_",end,".csv",sep='')
  write.csv(quote,file=save)
  return(invisible(quote))
}