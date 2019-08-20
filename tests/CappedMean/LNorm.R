
#set seed and number of simulations
set.seed(1)
n=1000*1000


#set parameters
cap=3000
mu=6.5
sigma=1.7


#Capped mean
x<-rlnorm(n, mu, sigma)
hist(x, breaks = 2000, xlim = c(0,1.5e4))
y<-ifelse(x<cap,x,cap)
hist(y)
mean(y)
LNormCappedMean(cap,mu,sigma)


#exposure curve
mean(y)/mean(x)
ExposureCurveLNorm(cap,mu,sigma)


#ILF
z<-ifelse(y>cap/2,cap/2,y)
mean(y)/mean(z)
ILFLNorm(cap/2,cap,mu,sigma)
ILFLNorm(cap/2,c(cap,cap*1.1),mu,sigma)



