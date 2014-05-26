data(rwalk)
plot(rwalk,type='o',ylab='Random Walk')
data(ar2.s)
plot(ar2.s,ylab=expression(Y[t]),type='o')
summary(ar2.s)
sd(ar2.s)^2
ph1=1.5
ph2=-.75
r0=(1-ph2)/(1+ph2)*1/((1-ph2)^2-ph1^2)
r0
# compare the autocorrelate function of sample with my computed results
ac=acf(ar2.s,type="correlation")
ac
r1=ph1/(1-ph2)
r2=(ph2*(1-ph2)+ph1^2)/(1-ph2)
r1;r2