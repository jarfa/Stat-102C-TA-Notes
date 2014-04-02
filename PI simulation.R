#Let's assume you already know a bit about R. This exercise should serve 
#as a review, and teach you about vectorizing your code.

#Problem: we want to estimate the value of PI. 
#One way to do this is to estimate the ratio of the area of a square to
#the area of a circle inscribed in that square. Note that if we have a 
#circle of radius r and a square of side lenght 2*r, the ratio of their 
#areas is: (PI*r^2) / (2*r)^2 = PI/4. 
#So, 4*((area of circle)/(area of square)) = PI.
#How do we estimate those areas? Through sampling. The number of samples 
#inside a given shape is proportional to its area.

#We'll go over two ways to code this: un-vectorized & vectorized.
#Un-Vectorized Code
r = 1
Nsim = 1e4
set.seed(20)
num_circle = 0
for(i in 1:Nsim){
  x = runif(1, min=-r, max=r)
  y = runif(1, min=-r, max=r)
  if(x^2 + y^2 < r)
    num_circle = num_circle + 1 
}
#Note that due to the way we sampled our x and y coords, every sample is within the box.
#so we're dividing by Nsim
print(4*num_circle / Nsim)
#repeat with higher levels of Nsim to get better estimates of PI. 
#But with higher levels of Nsim, the computation gets REALLY slow.

#Vectorized Code
r = 1
Nsim = 1e4
#set.seed(20)
x = runif(Nsim, min=-r, max=r)
y = runif(Nsim, min=-r, max=r)
in_circle = (x^2 + y^2) < r^2
num_circle = sum(in_circle)
print(4*num_circle / Nsim)
#Try it at Nsim = 1e6 or 1e7. Notice how much faster that is for higher levels of Nsim?

#Let's plot our samples
plot(x, y, col = in_circle)
#That's not right... it's interpreting TRUE/FALSE as black/white
color = ifelse(in_circle, 'blue', 'red')
plot(x, y, col=color)
library(ggplot2)
circle_plotdata = data.frame(x, y, color)
ggplot(data = circle_plotdata, aes(x=x, y =y, color = color)) +
  geom_point(alpha = 0.7) + guides(color = FALSE) + theme_bw() +
  labs(title = "My Plot")



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
#Estimates = sapply(N, PIsim) #a vectorized way to do the above for loop
#This might be too advanced for the first lecture, but it would be good
#to learn how to use the apply family of functions and/or the plyr library.
plot(log10(N), Estimates, type = 'l')

plotdata = data.frame(N, Estimates)
ggplot(plotdata, aes(x=N, y = Estimates)) + geom_line(color = 'blue') +
  scale_x_log10() + theme_bw()
#ggplot(data = plotdata, aes(x = N, y = Estimates)) + geom_line(color = 'blue') +
#  scale_x_log10() + theme_bw()

#Speed Comparison - vectorized vs. unvectorized
PIsim_unvect = function(Nsim){
  r = 1
  num_circle = 0
  for(i in 1:Nsim){
    x = runif(1, min=-r, max=r)
    y = runif(1, min=-r, max=r)
    if(x^2 + y^2 < r)
      num_circle = num_circle + 1 
  }
  return(4*num_circle / Nsim)
}
N = 10^(1:6)
times = data.frame(N=N, vect = numeric(6), unvect = numeric(6))
#Again, there are faster ways than a for loop. But this is a pretty short
#loop, so it's not a huge deal.
for(i in 1:length(N)){
  times$vect[i] = system.time(PIsim(times$N[i]))[3]
  times$unvect[i] = system.time(PIsim_unvect(times$N[i]))[3]
}
times
cbind(times$N, times$unvect / times$vect)
plot(log10(times$N), times$unvect, type='l')
lines(log10(times$N), times$vect)
library(reshape2)
plotdata = melt(times, measure.vars = c('vect', 'unvect'))
ggplot(plotdata, aes(x = N, y = value, color = variable)) + 
  geom_line() + scale_x_log10() + scale_y_log10()
