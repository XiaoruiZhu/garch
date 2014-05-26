##系统的GARCH()函数MLE求法求出来的
xitong<-function(h,x){
  M.N<-garch(x,order<-c(1,1),trace<-FALSE)
  return(M.N)
}