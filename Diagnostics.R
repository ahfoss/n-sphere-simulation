source('GenSphereNorm.R')
source('GenSphereSE.R')
source('GenSphereRejection.R')

sink('diagnostics')

set.seed(1)

# plot 2D data
plot(genSphereSE(500,2))
plot(genSphereNorm(500,2))
plot(genSphereRejection(500,2))

# Inspect slices of 4D data; should be approx uniform circular
circData4A <- genSphereSE(5000,4)
circData4B <- genSphereNorm(5000,4)
rejData4 <- genSphereRejection(5000,4)

sliceCoordsA <- (
  abs(circData4A[,3]) < 0.2
  & abs(circData4A[,4]) < 0.2
)
plot(circData4A[sliceCoordsA,1],circData4A[sliceCoordsA,2])

sliceCoordsB <- (
  abs(circData4B[,3]) < 0.2
  & abs(circData4B[,4]) < 0.2
)
plot(circData4B[sliceCoordsB,1],circData4B[sliceCoordsB,2])

sliceCoords2 <- (
  abs(rejData4[,3]) < 0.2
  & abs(rejData4[,4]) < 0.2
)
plot(rejData4[sliceCoords2,1],rejData4[sliceCoords2,2])

# Inspect 5D bivariate marginals; should show larger density towards center
circData5A <- genSphereSE(500,5,center=1:5,r=3)
circData5B <- genSphereNorm(500,5,center=1:5,r=3)
rejData5 <- genSphereRejection(500,5,center=1:5,r=3)

pairs(circData5A,col=rgb(r=0,g=0,b=1,a=.5))
pairs(circData5B,col=rgb(r=0,g=0,b=1,a=.5))
pairs(rejData5,col=rgb(r=0,g=0,b=1,a=.5))

# Inspect marginals of 5D data; should look parabolic
orig <- par(mfrow=c(2,2))
hist(circData5A[,1],breaks=15)
hist(circData5B[,1],breaks=15)
hist(rejData5[,1],breaks=15)
plot.new()
hist(circData5A[,2],breaks=15)
hist(circData5B[,2],breaks=15)
hist(rejData5[,2],breaks=15)
plot.new()
hist(circData5A[,3],breaks=15)
hist(circData5B[,3],breaks=15)
hist(rejData5[,3],breaks=15)
plot.new()
hist(circData5A[,4],breaks=15)
hist(circData5B[,4],breaks=15)
hist(rejData5[,4],breaks=15)
plot.new()
hist(circData5A[,5],breaks=15)
hist(circData5B[,5],breaks=15)
hist(rejData5[,5],breaks=15)
par(orig)

sink()
