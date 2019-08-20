
#set seed and number of simulations
set.seed(1)
n=1000*1000


#set gamma parameters
cap=10000
shape=1.5
rate=0.0004


#Capped gamma mean
x<-rgamma(n, shape, rate)
hist(x)
y<-ifelse(x<cap,x,cap)
mean(y)
hist(y)
GammaCappedMean(cap,shape,rate)


#exposure curve gamma
mean(y)/mean(x)
ExposureCurveGamma(cap,shape,rate)


#ILF Gamma
z<-ifelse(y>cap/2,cap/2,y)
mean(y)/mean(z)
ILFGamma(cap/2,cap,shape, rate)
ILFGamma(cap/2,c(cap,cap*1.1),shape, rate)



