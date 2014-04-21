#regular MC integration vs. importance sampling

#What's the E(x^2) when X~N(0,1)?
Nsim = 5e4
h = function(x) x^2
x = rbeta(Nsim, 5, 5)
I = h(x)
par(mfrow = c(2,1))
hist(x, breaks=40)
hist(I, breaks=40)
#think of this next line like a mean of all the samples up to each iteration
estint_regular = cumsum(I)/(1:Nsim) 
estint_regular[Nsim]
mean(I)
esterr = sqrt(cumsum((x-estint_regular)^2))/(1:Nsim)
par(mfrow = c(1,1))
plot(estint_regular, main="Mean and error range",type="l", xlab='Iteration',
     lwd=2, ylim=mean(I)+20*c(-esterr[Nsim],esterr[Nsim]), ylab='')
lines(estint_regular+2*esterr,col="gold",lwd=2)
lines(estint_regular-2*esterr,col="gold",lwd=2)

#Again, but with importance sampling. 
#g(x)~U(-3,3), f(x) ~ N(0,1)
#E(h(x)) = 1/n * sum( f(x)/g(x) *h(x) )
Nsim = 5e4
h = function(x) x^2
x = runif(Nsim)
g = function(x) dunif(x)
f = function(x) dbeta(x, 5, 5)
I = f(x)/g(x) * h(x)
par(mfrow = c(2,1))
hist(x, breaks=40)
hist(I, breaks=40)#are surprised that this looks different than up above?
#think of this next line like a mean of all the samples up to each iteration
estint_importance = cumsum(I)/(1:Nsim) 
estint_importance[Nsim]
mean(I)
esterr = sqrt(cumsum((x-estint_importance)^2))/(1:Nsim)
par(mfrow = c(1,1))
plot(estint_importance, main="Mean and error range",type="l", xlab='Iteration',
     lwd=2, ylim=mean(I)+20*c(-esterr[Nsim],esterr[Nsim]), ylab='')
lines(estint_importance+2*esterr,col="gold",lwd=2)
lines(estint_importance-2*esterr,col="gold",lwd=2)

plot(y = estint_regular, x = 1:Nsim, type='l', col='blue',
     ylab = 'Iteration', xlab = 'Estimate', log='x',
     ylim=c(0.1, max(estint_regular, estint_importance)))
#try this with and without the "log = 'x'" parameter
lines(y = estint_importance, x = 1:Nsim, col='red')
abline(h=estint_regular[Nsim], lty=2, lwd=2, col='grey')

