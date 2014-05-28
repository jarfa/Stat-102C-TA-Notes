#Gibbs Sampling from Bivariate Normal
#inspired by http://www.stat.wisc.edu/~mchung/teaching/stat471/lecture23.pdf
#Distribution is Normal with mean (0,0), covariance matrix of
#(1, rho)
#(rho, 1)

Nsim = 500
x = y = numeric(Nsim)
x[1] = 3
y[1] = -3 #try (x,y) = (10,10) to demonstrate the burn-in effect
rho = 0.9; #correlation coefficient
for(i in 2:Nsim){
  x[i] = rnorm(1, rho*y[i-1], sqrt(1-rho^2)) #conditional dist. of x|y
  y[i] = rnorm(1, rho*x[i], sqrt(1-rho^2)) #conditional dist. of y|x
}
plot(x,y)
lines(x,y, type='l', col='red', lwd=0.5)
