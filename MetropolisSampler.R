geneq = function(x){ #this function gets numbers from our proposal distribution q
  rnorm(1, mean=x, sd = 0.1)
}
q = function(x, y){ #the pdf of q
  dnorm(y, mean=x, sd=0.1)
}
f = function(x){ #the pdf of f - the target distribution
  dnorm(x, mean=0, sd = 1)
}

Nsim = 3e5
x = rep(0, Nsim) #start at x[1]==0... this doesn't matter too much
for(t in 1:(Nsim-1)){
  y = geneq(x[t])
  r = runif(1)
  # if(r < min(1, f(y)*q(y,x[t])/(f(x[t])*q(x[t],y))))
  if(r < min(1, f(y) / f(x[t])) ) #same as above if q(x,y) == q(y,x), i.e. q is symmetric
  {
    x[t+1] = y
  } else x[t+1] = x[t]
}
hist(x, freq=FALSE)
curve(dnorm(x), add=TRUE, col='blue')

qqnorm(x, main='QQplot of Metropolis Samples'); qqline(x, lwd=3, col='red')
#how does this compare to normal data generated directly from rnorm()?
y=rnorm(Nsim); qqnorm(y, main='QQplot of rnorm() Samples'); qqline(y, lwd=3, col='red')
#What if f() is not uni-modal?
#Let's say that f() is rbeta(1/4, 1/4). We'll keep our q as normal, but narrow it to SD=0.25
geneq = function(x){#this function gets numbers from our proposal distribution q
  rnorm(1, mean=x, sd=0.25)
}
q = function(x, y){#the pdf of q
  dnorm(y, mean=x, sd=0.25)
}
f = function(x){#the pdf of f - the target distribution
  dbeta(x, .25, .25)
}
Nsim = 1e5
x = rep(0.5, Nsim)
#start at 0.5 because if we start outside at 0 or 1 (or outside those bounds) it will never move - f(x) will always be 0
for(t in 1:(Nsim-1)){
  y = geneq(x[t])
  r = runif(1)
  # if(r < min(1, f(y)*q(y,x[t])/(f(x[t])*q(x[t],y))))
  if(r < min(1, f(y) / f(x[t])) ) #same as above if q(x,y) == q(y,x), i.e. q is symmetric
  {
    x[t+1] = y
  } else x[t+1] = x[t]
}
hist(x, freq=FALSE)
curve(dbeta(x, 1/4, 1/4), add=TRUE, col='blue')
