#This code is supposed to provide you with some MAJOR hints on how to 
#code HW1 Q4. The ONLY differences are the target and candidate distributions.

#Instead of N(0,1) truncated at C, we're using Gamma(2, 0.5)
#Why did I choose this distribtuion? Because it looks vaguely normal.
par(mfrow = c(1,1))
curve(dgamma(x, 2, 0.5),0, 20, lwd=2)
#Let's say that C>=3, just like in the HW example C>=0
abline(v=3, col='blue')

#Instead of exp(lambda), we're using Gamma(1, beta)
#Why did I choose this distribtuion? Because it looks vaguely 
#like exp(L)
betavals = c(1, 2, 5, 10)
for(i in 1:4){
  curve(dgamma(x, 1, betavals[i]), 0, 5, 
        add=ifelse(i==1,FALSE,TRUE), 
        col=i, lwd=2, ylab='Density', 
        main = 'Gamma Densities')
}
legend('topright', paste0('a,b = 1,',betavals), lty=1, col=1:4)

#PDF of truncated Gamma(a=2,b=0.5)
f = function(x, C){ 
  ifelse(x >= C, 
         dgamma(x, 2, 0.5) / (1 - pgamma(C, 2, 0.5)), 
         0)
}
#PDF of Gamma(a=1, b=betastar)
g = function(x, betastar, C){
  ifelse(x >=C, 
         dgamma((x-C), 1, betastar),
         0)
}
M_funct = function(betastar,C){
  stopifnot(C >= 3)
  optimize(f = function(x){f(x,C) / g(x, betastar, C)},
           interval = c(C, 35), maximum=TRUE)$objective
}
#let's try this at C=4
Cstar = 4
optimum = optimize(f = function(B) M_funct(B, C=Cstar), 
                   interval = c(0.1, 5), maximum=FALSE)
print(opt_beta <- optimum$minimum)
print(M_min <- optimum$objective)
#just to visualize what we just did...
B = seq(0.1, 1, by=0.01)
M_opt = numeric(length(B))
for(i in 1:length(B)) 
  M_opt[i] = M_funct(B[i], Cstar)
plot(B, M_opt, type='l', xlab = 'Beta', log='y',
     main = paste0('Optimal M per Beta (C=',Cstar,')'))
abline(v=opt_beta, col='red', lty=2,lwd=2)
abline(h=M_min, col='blue', lty=2,lwd=2)

#OK! Let's wrap everything we need to do into a function.
#We'll give it the number of samples we need, the function 
#to optimize, and C.
gen_truncGam = function(nsamples, Mfunct, C){
  #finds lambda and M
  optimum = optimize(f = function(B) M_funct(B, C), 
                     interval = c(0.1, 5), maximum=FALSE)
  beta = optimum$minimum
  M_min = optimum$objective
  nsim = nsamples
  #start with num_samples, add 1 for each additional simulation
  target = numeric(nsamples)
  for(i in 1:nsamples){
    u = runif(1) * M_min
    y = rgamma(1, 1, beta) + C
    while(u > f(y, C)/g(y, beta, C)){
      u = runif(1) * M_min
      y = rgamma(1, 1, beta) + C
      nsim = nsim+1
    }
    target[i] = y
  }
  return(list(
    x=target, 
    accept = nsamples/nsim, 
    M = M_min, 
    beta = beta
  ))
}

N = 1e4
Cvals = c(3.1, 3.5, 5, 8)
par(mfrow = c(2,2))
for(Cstar in Cvals){
#Note: uncomment the 2 commented lines below to see how our samples fit the target distribution
  truncGam = gen_truncGam(N, M_funct, C = Cstar)
  hist(truncGam$x, xlim = c(0, max(truncGam$x)), 
#        freq=FALSE,
       xlab = paste0('Truncated Gamma(2,0.5) @ C = ', Cstar), 
       main = paste0('Acceptance: ', round(truncGam$accept,2), 
                     ', 1/M = ', round(1/truncGam$M,2)))
#   curve(f(x, Cstar), add=TRUE, col='blue') #curve(dgamma(x, 2, 0.5) / (1-pgamma(Cstar,2,0.5)), add=TRUE, col='blue')
}
