## Generate n=101 equidistant points in [-2\pi, 2\pi]
m <- 1000
n <- 101
x <- 2*pi*seq(-1, 1, length=n)
summary(x)

mexhat<-function(xi){
  return((1-(xi**2))*exp(-0.5*(xi**2)))
}
fx<-mexhat(x)
summary(fx)
plot(fx,type="l",ylab="Response") #True Mexican Hat
## Initialize the matrix of fitted values for three methods
fvlp <- fvnw <- fvss <- matrix(0, nrow= n, ncol= m)
##Generate data, fit the data and store the fitted values
set.seed(79)
for (j in 1:m){
  ## simulate y-values
  ## Note that you need to replace $f(x)$ below by the mathematical definition in eq. (2)
  y <- fx + rnorm(length(x),mean=0,sd=0.2);
  ## Get the estimates and store them
  fvlp[,j] <- predict(loess(y ~ x, span = 0.75), newdata = x);
  fvnw[,j] <- ksmooth(x, y, kernel="normal", bandwidth= 0.2, x.points=x)$y;
  fvss[,j] <- predict(smooth.spline(y ~ x), x=x)$y
  
}

## Below is the sample R code to plot the mean of three estimators in a single plot
meanlp = apply(fvlp,1,mean);
meannw = apply(fvnw,1,mean);
meanss = apply(fvss,1,mean);
par(mfrow=c(3,3))
plot(meanlp)
plot(meannw)
plot(meanss)

biaslp=meanlp - fx
varlp= apply((fvlp - meanlp)**2,1,mean)
mselp= apply((fvlp - fx)**2,1,mean)

biasnw=meannw - fx
varnw= apply((fvnw - meannw)**2,1,mean)
msenw= apply((fvnw - fx)**2,1,mean)

biasss=meanss - fx
varss= apply((fvss - meanss)**2,1,mean)
msess= apply((fvss - fx)**2,1,mean)
mean(msess)

mean(biaslp)
mean(biasnw)
mean(biasss)

mean(varlp)
mean(varnw)
mean(varss)

###MEAN of the 3 Estimators in a single plot###
par(mfrow=c(1,1))
dmin = min( meanlp, meannw, meanss);
dmax = max( meanlp, meannw, meanss);
matplot(x, meanlp, "l", ylim=c(dmin, dmax), ylab="Response")
matlines(x, meannw, col="red")
matlines(x, meanss, col="blue")
## You might add the raw observations to compare with the fitted curves
# points(x,y)
## Can you adapt the above codes to plot the empirical bias/variance/MSE?

par(mfrow=c(1,1))
###BIAS PLOT###
dmin = min( biaslp, biasnw, biasss);
dmax = max( biaslp, biasnw, biasss);
matplot(x, biaslp, "l", ylim=c(dmin, dmax), ylab="Response")
matlines(x, biasnw, col="red")
matlines(x, biasss, col="blue")

###VAR PLOT###
dmin = min( varlp, varnw, varss);
dmax = max( varlp, varnw, varss);
matplot(x, varlp, "l", ylim=c(dmin, dmax), ylab="Response")
matlines(x, varnw, col="red")
matlines(x, varss, col="blue")

###MSE PLOT###
dmin = min( mselp, msenw, msess);
dmax = max( mselp, msenw, msess);
matplot(x, mselp, "l", ylim=c(dmin, dmax), ylab="Response")
matlines(x, msenw, col="red")
matlines(x, msess, col="blue")


###REPEAT PART (1) WITH ANOTHER (DETERMINISTIC) DESIGN THAT HAS NON-EQUIDISTANT POINTS.###
set.seed(79)
x <- 2*pi*sort(c(0.5, -1 + rbeta(50,2,2), rbeta(50,2,2)))

mexhat<-function(xi){
  return((1-(xi**2))*exp(-0.5*(xi**2)))
}
fx<-mexhat(x)
plot(fx,type="l",ylab="Response") #True Mexican Hat
## Initialize the matrix of fitted values for three methods
fvlp <- fvnw <- fvss <- matrix(0, nrow= n, ncol= m)
##Generate data, fit the data and store the fitted values
set.seed(79)
for (j in 1:m){
  ## simulate y-values
  ## Note that you need to replace $f(x)$ below by the mathematical definition in eq. (2)
  y <- fx + rnorm(length(x),mean=0,sd=0.2);
  ## Get the estimates and store them
  fvlp[,j] <- predict(loess(y ~ x, span = 0.3365), newdata = x);
  fvnw[,j] <- ksmooth(x, y, kernel="normal", bandwidth= 0.2, x.points=x)$y;
  fvss[,j] <- predict(smooth.spline(y ~ x,spar = 0.7163), x=x)$y
  
}

## Below is the sample R code to plot the mean of three estimators in a single plot
meanlp = apply(fvlp,1,mean);
meannw = apply(fvnw,1,mean);
meanss = apply(fvss,1,mean);

biaslp=meanlp - fx
varlp= apply((fvlp - meanlp)**2,1,mean)
mselp= apply((fvlp - fx)**2,1,mean)

biasnw=meannw - fx
varnw= apply((fvnw - meannw)**2,1,mean)
msenw= apply((fvnw - fx)**2,1,mean)

biasss=meanss - fx
varss= apply((fvss - meanss)**2,1,mean)
msess= apply((fvss - fx)**2,1,mean)

mean(mselp)
mean(msenw)
mean(msess)


###MEAN of the 3 Estimators in a single plot###
dmin = min( meanlp, meannw, meanss);
dmax = max( meanlp, meannw, meanss);
matplot(x, meanlp, "l", ylim=c(dmin, dmax), ylab="Response")
matlines(x, meannw, col="red")
matlines(x, meanss, col="blue")
## You might add the raw observations to compare with the fitted curves
# points(x,y)
## Can you adapt the above codes to plot the empirical bias/variance/MSE?

###BIAS PLOT###
dmin = min( biaslp, biasnw, biasss);
dmax = max( biaslp, biasnw, biasss);
matplot(x, biaslp, "l", ylim=c(dmin, dmax), ylab="Response")
matlines(x, biasnw, col="red")
matlines(x, biasss, col="blue")

###VAR PLOT###
dmin = min( varlp, varnw, varss);
dmax = max( varlp, varnw, varss);
matplot(x, varlp, "l", ylim=c(dmin, dmax), ylab="Response")
matlines(x, varnw, col="red")
matlines(x, varss, col="blue")

###MSE PLOT###
dmin = min( mselp, msenw, msess);
dmax = max( mselp, msenw, msess);
matplot(x, mselp, "l", ylim=c(dmin, dmax), ylab="Response")
matlines(x, msenw, col="red")
matlines(x, msess, col="blue")

###Code to find optimal spline###
# xyspline <- smooth.spline(y~x, cv=T)
# xyspline$spar

