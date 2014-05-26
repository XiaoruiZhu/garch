### 验证不同的参数设定对超额峰度有影响
a1=c(0.5,0.6);b1=0.3;
a2=c(0.02,0.05);b2=0.9
a3=c(0.02,0.1);b3=0.8
x1=garch.sim(a1,b1,1000)
k1=kurtosis(x1)
x2=garch.sim(a2,b2,1000)
k2=kurtosis(x2)
x3=garch.sim(a3,b3,1000)
k3=kurtosis(x3)
k1;k2;k3
library(fBasics)
normalTest(x1,method='jb')
normalTest(x2,method='jb')
normalTest(x3,method='jb')
### 新息的t分布变化对超额峰度的影响
x22=T.GARCH(a2,b2,1000,dft=3);k22=kurtosis(x22)
x32=T.GARCH(a3,b3,1000,dft=3);k32=kurtosis(x32)
k22;k32
k22
normalTest(x22,method='jb')#正态检验，用JB统计量，结合了偏度和峰度的检验
normalTest(x32,method='jb')
