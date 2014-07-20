# Based on http://stats.stackexchange.com/questions/7977/how-to-generate-uniformly-distributed-points-on-the-surface-of-the-3-d-unit-sphe

library(mvtnorm)

genSphereNorm <- function(nrPoints,nrDim,center=rep(0,nrDim),r=1){
  xs <- rmvnorm(nrPoints,sigma=diag(nrDim))
  lambda <- apply(xs,1,function(tt) sqrt(sum(tt^2)))
  # multiply by r instead of outer(...) for coordinates on surface of sphere
  sph <- (xs / lambda) * outer(runif(nrPoints,0,r^nrDim)^(1/nrDim),rep(1,nrDim))
  sph <- sph + outer(rep(1,nrPoints),center)
}
