#Let's assume you already know a bit about R. This exercise should serve 
#as a review, and teach you about vectorizing your code.

#Problem: we want to estimate the value of PI. 
#One way to do this is to estimate the area of a square and a circle 
#inscribed in that square. Note that if we have a circle of radius r
#and a square of side lenght 2*r, the ratio of their areas is:
#(PI*r^2) / (4*r^2) = PI/4. So, 4*((area of circle)/(area of square)) = PI.
#How do we estimate those areas? Through sampling. The number of samples 
#inside each shape are proportional to its area

#Un-Vectorized Code
r = 1
Nsim = 1e6
set.seed(20)
num_circle = 0
for(i in 1:Nsim){
  x = runif(1, min=-r, max=r)
  y = runif(1, min=-r, max=r)
  if(x^2 + y^2 < r)
    num_circle = num_circle + 1 
}
#Note that due to the way we sampled our x and y coords, every sample is within the box
print(4*num_circle / Nsim)
#repeat with higher levels of Nsim to get better estimates of PI

#Vectorized Code
r = 1
Nsim = 1e7
#set.seed(20)
x = runif(Nsim, min=-r, max=r)
y = runif(Nsim, min=-r, max=r)
in_circle = (x^2 + y^2) < r^2
num_circle = sum(in_circle)
print(4*num_circle / Nsim)
#Notice how much faster that is?

#Let's plot our samples
color = ifelse(in_circle, 'blue', 'red')
plot(x, y, col=color)

#Wrap our vectorized code into a function, then we can see how our 
#estimate of PI changes over Nsim
PIsim = function(Nsim){
  r = 1
  x = runif(Nsim, min=-r, max=r)
  y = runif(Nsim, min=-r, max=r)
  num_circle = sum((x^2 + y^2) < r^2)
  return(4*num_circle / Nsim)
}
N = 10^(1:7)
set.seed(15)
Estimates = numeric(length(N))
for(i in 1:length(N))
  Estimates[i] = PIsim(N[i])
#Estimates = sapply(N, PIsim)
plot(log10(N), Estimates, type = 'l')
