genSphereNorm <- function(nrPoints,nrDim,center=rep(0,nrDim),r=1){
  xs <- rmvnorm(nrPoints,sigma=diag(nrDim))
  lambda <- apply(xs,1,function(tt) sqrt(sum(tt^2)))
  # multiply by r instead of outer(...) for coordinates on surface of sphere
  sph <- (xs / lambda) * outer(runif(nrPoints,0,r^nrDim)^(1/nrDim),rep(1,nrDim))
  sph <- sph + outer(rep(1,nrPoints),center)
}
