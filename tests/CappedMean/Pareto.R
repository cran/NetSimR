
#set seed and number of simulations
set.seed(1)
n=1000*1000


#set parameters
cap=5000
scale=2000
shape=1.7


#Capped mean
x<-scale/(runif(n)^(1/shape))
hist(x, breaks = 20000, xlim = c(0,2e4))
y<-ifelse(x<cap,x,cap)
hist(y)
mean(y)
ParetoCappedMean(cap,scale,shape)


#exposure curve
mean(y)/mean(x)
ExposureCurvePareto(cap,scale,shape)


#ILF
z<-ifelse(y>cap/2,cap/2,y)
mean(y)/mean(z)
ILFPareto(cap/2,cap,scale,shape)
ILFPareto(cap/2,cap,c(scale,scale*1.1),shape)


