
#set seed and number of simulations
set.seed(1)
n=10*1000*1000

#set gamma parameters
gshape=1.5
rate=0.0004
SlicePoint=400
shape=1.5

#test pdf
x<-rgamma(n,gshape,rate)
x<-ifelse(x<SlicePoint,x,SlicePoint/(runif(n)^(1/shape)))
hist(x, breaks = 200000, xlim = c(0,1.5e4), probability = T)
lines(0:1e4,dSlicedGammaPareto(0:1e4,gshape,rate,SlicePoint,shape), col="red")

#test cdf
step<-0.001
plot(log(quantile(x,seq(0+step, 1-step, step)))
     ,pSlicedGammaPareto(quantile(x,seq(0+step, 1-step, step)),gshape,rate,SlicePoint,shape)
     ,type = "l"
)
lines(log(quantile(x,seq(0+step, 1-step, step))),seq(0+step, 1-step, step), type = "l", col="red")


#test inverese cdf
plot(log(qSlicedGammaPareto(seq(0+step, 1-step, step),gshape,rate,SlicePoint,shape))
     ,seq(0+step, 1-step, step)
     ,type = "l"
)
lines(log(quantile(x,seq(0+step, 1-step, step))),seq(0+step, 1-step, step), type = "l", col="red")

