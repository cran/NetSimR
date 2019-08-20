
#set seed and number of simulations
set.seed(1)
n=10*1000*1000

#set parameters
cap=5000
mu=6.7
sigma=1.65
SlicePoint=3000
shape=1.5

#Capped mean
x<-rlnorm(n,mu,sigma)
x<-ifelse(x<SlicePoint,x,SlicePoint/(runif(n)^(1/shape)))
hist(x, breaks = 20000, xlim = c(0,1e4))
mean(x)
SlicedLNormParetoMean(mu,sigma,SlicePoint,shape)
y<-ifelse(x<cap,x,cap)
hist(y)
mean(y)
SlicedLNormParetoCappedMean(cap,mu,sigma,SlicePoint,shape)


#exposure
mean(y)/mean(x)
ExposureCurveSlicedLNormPareto(cap,mu,sigma,SlicePoint,shape)


#ILF
z<-ifelse(y>cap/2,cap/2,y)
mean(y)/mean(z)
ILFSlicedLNormPareto(cap/2,cap,mu,sigma,SlicePoint,shape)
ILFSlicedLNormPareto(cap/2,c(cap,cap*1.1),mu,sigma,SlicePoint,shape)

