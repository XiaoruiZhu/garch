# 并行计算euler14问题
# 自定义函数以返回原始数值和步数
func <- function(x) {
  n = 1
  raw <- x
  while (x > 1) {
    x <- ifelse(x%%2==0,x/2,3*x+1)
    n = n + 1
  }
  return(c(raw,n))
}

library(parallel)
# 用system.time来返回计算所需时间
system.time({
  x <- 1:1000000
  cl <- makeCluster(4)  # 初始化四核心集群
  results <- parLapply(cl,x,func) # lapply的并行版本
  res.df <- do.call('rbind',results) # 整合结果
  stopCluster(cl) # 关闭集群
})
# 找到最大的步数对应的数字##极其有用，及其重要
res.df[which.max(res.df[,2]),1]


#上例中关键的函数就是parLapply，其中三个参数分别是集群对象、输入参数和运算函数名。我们最后算出的结果是837799。

#foreach包是revolutionanalytics公司贡献给R开源社区的一个包。它能使R中的并行计算更为方便。与sapply函数类似，foreach函数中的第一个参数是输入参数，%do%后面的对象表示运算函数，而.combine则表示运算结果的整合方式。 下面的例子即是用foreach来完成前面的同一个任务。如果要启用并行，则需要加载doParallel包，并将%do%改为%dopar%。这样一行代码就能方便的完成并行计算了。

library(foreach)
# 非并行计算方式，类似于sapply函数的功能
system.time({
  x <- foreach(x=1:1000,.combine='rbind') %do% func(x)
})
# 启用parallel作为foreach并行计算的后端
library(doParallel)
system.time({
  cl <- makeCluster(4)
  registerDoParallel(cl)
  # 并行计算方式
  x <- foreach(x=1:1000,.combine='rbind') %dopar% func(x)
  stopCluster(cl)
})



#下面的例子是用foreach函数来进行随机森林的并行计算。我们一共要生成十万个树来组合成一个随机森林，每个核心负责生成两万五千个树。最后用combine进行组合。

# 随机森林的并行计算
library(randomForest)
cl <- makeCluster(4)
registerDoParallel(cl)
rf <- foreach(ntree=rep(25000, 4), 
              .combine=combine,
              .packages='randomForest') %dopar%
  randomForest(Species~., data=iris, ntree=ntree)
stopCluster(cl)

#并行不仅可以在建模时进行，也可以在数据整理阶段进行。之前我们提到过的plyr包也可以进行并行，前提是加载了foreach包，并且参数.parallel设置为TURE。当然不是所有的任务都能并行计算，而且并行计算前你需要改写你的代码。