
#set seed and number of simulations
set.seed(10)
n=10*1000*1000

#set parameters
cap=10*1000
gshape=1.5
rate=0.0004
SlicePoint=5000
shape=1.5

#Capped mean
x<-rgamma(n,gshape,rate)
x<-ifelse(x<SlicePoint,x,SlicePoint/(runif(n)^(1/shape)))
hist(x, breaks = 200000, xlim = c(0,3e4))
mean(x)
SlicedGammaParetoMean(gshape,rate,SlicePoint,shape)
y<-ifelse(x<cap,x,cap)
hist(y)
mean(y)
SlicedGammaParetoCappedMean(cap,gshape,rate,SlicePoint,shape)


#exposure curve
mean(y)/mean(x)
ExposureCurveSlicedGammaPareto(cap,gshape,rate,SlicePoint,shape)


#ILF
z<-ifelse(y>cap/2,cap/2,y)
mean(y)/mean(z)
ILFSlicedGammaPareto(cap/2,cap,gshape,rate,SlicePoint,shape)
ILFSlicedGammaPareto(cap/2,c(cap,cap*1.1),gshape,rate,SlicePoint,shape)

