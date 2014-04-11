#Rejection Sampling AKA Accept/Reject Sampling
#J Arfa
#http://en.wikipedia.org/wiki/Rejection_sampling

#Let's get a normal distrubtion from a uniform distribution using 
#another uniform distribution for the candidate/proposal distribution.
N = 1e4
gsamples = runif(N, -4,4)
hist(gsamples, freq=FALSE, ylim = c(0,0.6))
curve(dnorm(x), col = 'red', add = T)
#M = 3.25 would make this work
optimize(f = function(x) dnorm(x)/0.125, interval = c(-4,4), maximum=TRUE)$objective
#M = 3.25 would make this work
M = 3.2
abline(h = 3.25 * .125, col='blue')

curve(dnorm(x), col = 'red', from=-4, to=4)
curve(dnorm(x) / (3.25*.125), col='red', from=-4, to=4)


samplefrom = function(gsamples){
  y = sample(gsamples, 1)
  u = runif(1)
  #     u > f(y)     /   g(y)
  while(u > dnorm(y) / (M*dunif(y, min=-4, max=4))){
    u = runif(1)
    y = sample(gsamples, 1) 
  }
  return(y)
}
normalsamples = replicate(1e3, samplefrom(gsamples) )
hist(normalsamples, freq=FALSE)
curve(dnorm(x), col = 'red', lwd=2,add = T)
#looks good!

#For reasons that will become clear, the closer your candidate 
#dist. gets to your target dist., the higher your acceptance rate.
#Here we'll compare how to generate a beta dist. from either a unif or beta
library(mcsm)
betagen(Nsim=1e3)
#we'll go through how these plots were made

Nsim = 1e3
a = 2.7
b = 6.3
optimize(f = function(x) dbeta(x, a, b), interval=c(0,1), 
         maximum=TRUE)$objective
#The above line shows the maximum point of the beta dist. So M needs to be >= 2.67
M = 2.67
y = runif(Nsim)
u = runif(Nsim, max = M) #same as runif(Nsim) * M
par(mfrow = c(1,1))
plot(y, u, col = "grey", pch = 19, cex = 0.4, 
     ylab = expression(u.g(y)))
points(y[u < dbeta(y, a, b)], u[u < dbeta(y, a, b)], pch = 19, 
       cex = 0.4)
curve(dbeta(x, a, b), col = "sienna", lwd = 2, add = T) #target density
abline(h = M, col = "gold4", lwd = 2) #candidate density
sum(u < dbeta(y, a, b)) / Nsim
#~37% of samples we generated from the uniform were inside the target density
#Note: this number will vary each time you run this simulation

par(mfrow = c(1,1))
plot(density(rbeta(1e5, 2.7, 6.3)), col='red', main = 'Beta Desnsities', ylim = c(0,3))
lines(density(rbeta(1e5, 2, 6)), col='blue')
legend(x=0.35,y=3, c('a, b = 2.7, 6.3', 'a, b = 2, 6'), lty=1, col = c('red', 'blue'))
optimize(f=function(x){dbeta(x,2.7,6.3)/dbeta(x,2,6)},
         maximum=T,interval=c(0,1))$objective
#thats the max ratio of candidate density to target density
M = 1.68
y = rbeta(Nsim, 2, 6)
u = runif(Nsim, max = M)
labels = u < dbeta(y, a, b)/dbeta(y, 2, 6)
#par(mfrow = c(1, 2), mar = c(4, 4, 2, 1))
plot(y, u * dbeta(y, 2, 6), col = "grey", pch = 19, cex = 0.4, 
     ylab = expression(u.g(y)))
points(y[labels], u[labels] * dbeta(y[labels], 2, 6), pch = 19, 
       cex = 0.4)
curve(dbeta(x, a, b), col = "sienna", lwd = 2, add = T)
curve(M * dbeta(x, 2, 6), col = "gold4", lwd = 2, add = T)
sum(labels) / Nsim
#57% acceptance!

par(mfrow = c(1,1))
hist(y[labels], freq=FALSE)
curve(dbeta(x, a,b), add=TRUE, col='red', lwd=2)
#Our samples are indeed from the target distribution.

hist(rbeta(1e5, 7, 3), xlim = c(0,1), freq=FALSE)
hist(rbeta(1e5, 70, 30), xlim = c(0,1), freq=FALSE)