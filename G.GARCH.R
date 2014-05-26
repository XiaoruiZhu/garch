##' .. content for \description{} (Function to generate GARCH time series with generalized Gaussian distribution innovations) ..
##'
##' .. content for \details{} ..
##' @title G.GARCH
##' @param alpha :include the incept and the parameter of autoregression parts
##' @param beta : is a vector include the parameter of conditional variance
##' @param n :is the lenght of time series
##' @param rnd:
##' @param df.G:is the df of Genaralized Gaussian distribution.
##' @param ntrans burn-in size, i.e. number of initial simulated data to be discarded
##' @param ...
##' @return
##' @author Xiaorui.Zhu
G.GARCH=function (alpha, beta, n = 100, rnd = rpgnorm, df.G,ntrans = 100, ...)
{
  library(pgnorm)
  if (!missing(beta))
    p = length(beta)
  else p = 0
  if (!missing(alpha))
    q = length(alpha) - 1
  else stop("beta is missing!")
  if (q == 0)
    stop("Check model: q=0!")
  total.n = n + ntrans
  e = rnd(total.n,df.G)
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
  x[1:d] = rnd(d, df.G)
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
