## Inverse transform sampling
#Muzhou Liang
## Generate samples of a continuous dist
## Uniform to Exponential
Nsim = 10^4; lambda = 1
U = runif(Nsim)
X = -log(U) / lambda
Y = rexp(Nsim, rate = lambda)
par(mfrow = c(1,2))
hist(X, freq=F, main="Exp from Uniform")
curve(dexp(x), col = "red", add = T)
hist(Y, freq=F, main="Exp from R")
curve(dexp(x), col = "red", add = T)

## Uniform to Normal
## Box-Muller algorithm, an accurate normal generator
Nsim = 10^4
U1 = runif(Nsim); U2 = runif(Nsim)
X1 = sqrt(-2*log(U1)) * cos(2*pi*U2)
X2 = sqrt(-2*log(U1)) * sin(2*pi*U2)
par(mfrow = c(1,2))
hist(X1, freq=F)
curve(dnorm(x), col = 'red', add = T)
hist(X2, freq=F)
curve(dnorm(x), col = 'red', add = T)

## Genrate samples of a discrete dist
## Uniform to Poisson (when lambda is large)
#This is the version of Inverse CDF Sampling for a discrete distr.
Nsim = 10^4; 
lambda = 100
spread = 3*sqrt(lambda) # variance = lambda
t = round(seq(max(0,lambda-spread),
              lambda+spread,1))
prob = ppois(t, lambda)
par(mfrow = c(1,1))
plot(t, prob, pch=16, cex=0.5)
X = rep(0,Nsim)
for (i in 1:Nsim){
  u = runif(1)
  X[i] = t[1] + sum(prob<u) 
}
par(mfrow = c(1,1))
hist(X, freq=F, main="Pois from Uniform")


## Mixture representations (depending on conditional density)

## X|y ~ Pois(y) and Y ~ Gamma(n, beta)
## then X ~ Neg(n,p) where beta = (1-p)/p
Nsim = 10^4
n=6; p=.3
y = rgamma(Nsim, n, rate=p/(1-p))
x = rpois(Nsim, y)
hist(x, main="", freq=F, col="grey", breaks=40)
lines(1:50, dnbinom(1:50,n,p), col="red")


