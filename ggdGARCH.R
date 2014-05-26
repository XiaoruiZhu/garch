#随机数据的生成，包括高斯，t等随机误差项的GARCH(1,1)

T.GARCH=function (alpha, beta, n = 100, rnd = rt, dft,ntrans = 100, ...) 
{
  if (!missing(beta)) 
    p = length(beta)
  else p = 0
  if (!missing(alpha)) 
    q = length(alpha) - 1
  else stop("beta is missing!")
  if (q == 0) 
    stop("Check model: q=0!")
  total.n = n + ntrans
  e = rnd(total.n,dft)####这里需要写成 广义高斯分布的随机数！！！！
  x = double(total.n)
  sigt = x
  d = max(p, q)
  sigma2 = sum(alpha[-1])
  if (p > 0) 
    sigma2 = sigma2 + sum(beta)
  if (sigma2 > 1) 
    stop("Check model: it does not have finite variance")
  sigma2 = alpha[1]/(1 - sigma2)
  if (sigma2 <= 0) 
    stop("Check model: it does not have positive variance")
  x[1:d] = rnd(d, dft)
  sigt[1:d] = sigma2
  if (p == 0) {
    for (i in (d + 1):total.n) {
      sigt[i] = sum(alpha * c(1, x[i - (1:q)]^2))
      x[i] = e[i] * sqrt(sigt[i])
    }
  }
  else {
    for (i in (d + 1):total.n) {
      sigt[i] = sum(alpha * c(1, x[i - (1:q)]^2)) + sum(beta * sigt[i - (1:p)])
      x[i] = e[i] * sqrt(sigt[i])
    }
  }
  return(invisible(x[(ntrans + 1):total.n]))
  #list(xt=x[(ntrans + 1):total.n],sigt=sigt[(ntrans + 1):total.n])
}