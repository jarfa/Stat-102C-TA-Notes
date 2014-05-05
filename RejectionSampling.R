#Rejection Sampling AKA Accept/Reject Sampling
#J Arfa
#http://en.wikipedia.org/wiki/Rejection_sampling

#Let's get a normal distrubtion from a uniform distribution using 
#another uniform distribution for the candidate/proposal distribution.
N = 1e4
#samples from the distribution we're calling g (our candidate/envelope distribution)
gsamples = runif(N, -4,4)
hist(gsamples, freq=FALSE, ylim = c(0,0.6))
#f (our target distribution) is N(0,1)
curve(dnorm(x), col = 'red', add = T)
#M = max point of f(x)/g(x)
optimize(f = function(x) dnorm(x)/0.125, #note that the pdf is 1/8 in that range.
         interval = c(-4,4), maximum=TRUE)$objective
M = 3.2 #at least above max(f/g)
#our condition for acceptance is M*u < f(x)/g(x). 
#M*u needs to be above the maximum point of f/g.
curve(dnorm(x) / 0.125, -4, 4, col='red')
abline(h = M, col='blue')

samplefrom = function(gsamples){
  #define the f and g PDFs
  f = function(x) 
    dnorm(x)
  g = function(x) #this will be 1/8 in that range, 0 elsewhere.
    dunif(x, min=-4, max=4) 
  y = sample(gsamples, 1)
  u = runif(1)
  #     u > f(y)     /   M*g(y)
  #alternatively, we could've done M*u > f(y)/g(y). 
  #Note that the ">" means that this is when our condistion is rejected.
  while(u > f(y) / (M*g(y))){
    u = runif(1)
    y = sample(gsamples, 1) 
  }
  return(y)
}
#get 1e3 samples from f
normalsamples = replicate(1e3, samplefrom(gsamples) )
hist(normalsamples, freq=FALSE)
curve(dnorm(x), col = 'red', lwd=2,add = T)
#looks good!

#For reasons that will become clear, the closer your candidate 
#dist. gets to your target dist., the higher your acceptance rate.
#Here we'll compare how to generate a beta dist. from either a unif or beta
library(mcsm)
betagen(Nsim=1e3) #Note how that the grey region is smaller in the graph on the right.
#we'll go through how these plots were made

Nsim = 1e3
a = 2.7
b = 6.3
optimize(f = function(x) dbeta(x, a, b)/1, #f~beta(a,b); g~unif(0,1)
         interval=c(0,1), 
         maximum=TRUE)$objective
#The above line shows the maximum point of the beta dist. So M needs to be >= 2.67
M = 2.67
y = runif(Nsim)
u = runif(Nsim)*M
par(mfrow = c(1,1))
plot(y, u, col = "grey", pch = 19, cex = 0.4, 
     ylab = expression(u.g(y))) #these are ALL of the points
#these are points in our acceptance region. Note the condition u < dbeta(y, a, b)
points(y[u < dbeta(y, a, b)], u[u < dbeta(y, a, b)], pch = 19, 
       cex = 0.4) 
curve(dbeta(x, a, b), col = "sienna", lwd = 2, add = T) #target density
abline(h = M, col = "gold4", lwd = 2) #candidate density
sum(u < dbeta(y, a, b)) / Nsim
#~40% (this number will change a bit each time you do this) of samples we 
#generated from the uniform were inside the target density


par(mfrow = c(1,1))
plot(density(rbeta(1e5, 2.7, 6.3)), col='red', main = 'Beta Densities', ylim = c(0,3))
lines(density(rbeta(1e5, 2, 6)), col='blue')
legend('topright', c('Target', 'Candidate'), lty=1, col = c('red', 'blue'))
optimize(f=function(x){dbeta(x,2.7,6.3)/dbeta(x,2,6)},
         maximum=T,interval=c(0,1))$objective
#thats the max ratio of candidate density to target density
M = 1.68
y = rbeta(Nsim, 2, 6)
u = runif(Nsim) * M
labels = u < dbeta(y, a, b)/dbeta(y, 2, 6)
#this is a vector of TRUE/FALSE indicating whether it's accepted.
#par(mfrow = c(1, 2), mar = c(4, 4, 2, 1))
plot(y, u * dbeta(y, 2, 6), col = "grey", pch = 19, cex = 0.4, 
     ylab = expression(u.g(y)))
points(y[labels], u[labels] * dbeta(y[labels], 2, 6), pch = 19, 
       cex = 0.4)
curve(dbeta(x, a, b), col = "sienna", lwd = 2, add = T)
curve(M * dbeta(x, 2, 6), col = "gold4", lwd = 2, add = T)
sum(labels) / Nsim #note that sum(lables) is the same as the count of TRUEs.
#~57% acceptance!

par(mfrow = c(1,1))
hist(y[labels], freq=FALSE)
curve(dbeta(x, a,b), add=TRUE, col='red', lwd=2)
#Our samples are indeed from the target distribution.

#just to demonstrate what  beta distribtion is...
#If we get 7 heads & 3 tails, what's the PDF of this coin's true p? 
hist(rbeta(1e5, 7, 3), xlim = c(0,1), freq=FALSE)
#If we get 70 heads & 30 tails, what's the PDF of this coin's true p? 
hist(rbeta(1e5, 70, 30), xlim = c(0,1), freq=FALSE)
