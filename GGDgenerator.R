library(pgnorm)
y<-rpgnorm(10000,3)
hist(y,freq=F)
lines(density(y))
