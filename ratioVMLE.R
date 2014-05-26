ratioVMLE=function(alpha,beta,simdf,samplesize,repN) {
    alleste=finalsim(repn=repN,samples=samplesize,dfsim=simdf)
    nameALL=paste("t",simdf,".",samplesize,"ALL",repN)
    nameMSE=paste("t",simdf,".",samplesize,"MSE",repN)
    ## RMSE1000返回结果按照( 平均bestdf AtQ比例 2NGQ比例 RMSEofGQ tQ比例 )来排列
    MSE=RMSE1000(dat=alleste,a=alpha,b=beta)
    save(alleste, file = "~\\study\\GARCH\\simulation\\nameALL.R")
    save(MSE, file = "~\\study\\GARCH\\simulation\\nameMSE.R")
    RMSE=sqrt(MSE[-1])
    return(c(MSE[1],RMSE))
}
