genSphereRejection <- function(nrPoints,nrDim,center=rep(0,nrDim),r=1) {
  dat <- matrix(nrow=nrPoints,ncol=nrDim)
  nAccept <- 0
  while (nAccept < nrPoints) {
    samp <- runif(nrDim,-r-center,r+center)
    if ( sqrt(sum( (samp-center)^2 )) < r ) {
      nAccept = nAccept + 1
      dat[nAccept,] <- samp
    }
  }
  dat
}
