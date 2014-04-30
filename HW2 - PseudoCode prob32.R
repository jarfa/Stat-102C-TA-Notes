#Pseudo-code for HW2 3.2
x1 = rnewdist(Nsim, parameters)
x2 = rnewdist(Nsim, parameters)
x3 = rnewdist(Nsim, parameters)
X = cbind(x1, x2, x3)

f = function(x1, x2, x3){
  #remember that x1, ... are I.I.D.
  dnorm(x1) * dnorm(x2) * dnorm(x3)
}
g = function(x1, x2, x3){
  dnewdist(x1) * ...
}
M = apply(X, 1, function(x){max(x[1], ....)})
weights = apply(X, 1, function(x){
  f(...) / g(...)
})
indicators = M > C
mean(indicators * weights)