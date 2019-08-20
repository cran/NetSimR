
#set seed and number of simulations
set.seed(1)
n=10*1000*1000

#set gamma parameters
mu=6.7
sigma=1.65
SlicePoint=300
shape=1.5

#test pdf
x<-rlnorm(n,mu,sigma)
x<-ifelse(x<SlicePoint,x,SlicePoint/(runif(n)^(1/shape)))
hist(x, breaks = 200000, xlim = c(0,3e3), probability = T)
lines(0:1e4,dSlicedLNormPareto(0:1e4,mu,sigma,SlicePoint,shape), col="red")

#test cdf
step<-0.001
plot(log(quantile(x,seq(0+step, 1-step, step)))
     ,pSlicedLNormPareto(quantile(x,seq(0+step, 1-step, step)),mu,sigma,SlicePoint,shape)
     ,type = "l"
  )
lines(log(quantile(x,seq(0+step, 1-step, step))),seq(0+step, 1-step, step), type = "l", col="red")


#test inverese cdf
plot(log(qSlicedLNormPareto(seq(0+step, 1-step, step),mu,sigma,SlicePoint,shape))
     ,seq(0+step, 1-step, step)
     ,type = "l"
)
lines(log(quantile(x,seq(0+step, 1-step, step))),seq(0+step, 1-step, step), type = "l", col="red")

