genSphereSE <- function(nrPoints,nrDim,center=rep(0,nrDim),r=1){
    if (nrDim > 2) warning('Not correct for more than two dimensions')
    #generate the polar coordinates!
    x <-  matrix(runif(nrPoints*nrDim,-pi,pi),ncol=nrDim)
    x[,nrDim] <- x[,nrDim]/2
    #recalculate them to cartesians
    sin.x <- sin(x)
    cos.x <- cos(x)
    cos.x[,nrDim] <- 1  # see the formula for n.spheres

    y <- sapply(1:nrDim, function(i){
        if(i==1){
          cos.x[,1]
        } else {
          cos.x[,i]*apply(sin.x[,1:(i-1),drop=F],1,prod)
        }
    })*sqrt(runif(nrPoints,0,r^2))

    y <-  as.data.frame(
            t(apply(y,1,'+',center))
          )

    names(y) <- make.names(seq_len(nrDim))
    y
}
