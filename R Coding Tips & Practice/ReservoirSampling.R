#Reservoir Sampling
#http://en.wikipedia.org/wiki/Reservoir_sampling
#Problem: we want a way to sample from a vector of unknown length.
#Imagine that data points are coming at you one at a time, and you
#want to sample from the data in a random, representative way. How
#would you do that?

set.seed(50)
N = 1e5
sample_from = rnorm(N) #let's say our incoming data is normal
k = 1e3 #we want 1,000 samples at the end
sample_to = rep(NA, k)

#Yes, there are MUCH faster ways than a for loop. 
#But reservoir sampling is based on the idea that we don't know how big 
#our data is yet. Let's pretend that it's coming in one line at a time.
#first k samples: fill our reservoir (hence the name).
for(i in 1:k)
  sample_to[i] = sample_from[i]
#next (N-k) samples: replace values in our reservoir with
#a decreasing probability
for(i in (k+1):N){
  r = sample(1:i, 1)
  if(r <= k) 
    sample_to[r] = sample_from[i]
}
#Note that, while each successive sample is less likely to be added
#to our sample, it's also less likely to be replaced later.
#sanity check: did it fill the new vector?
sum(is.na(sample_to))
#Let's see what we have
par(mfrow = c(2,2))
hist(sample_from)
qqnorm(sample_from)
hist(sample_to)
qqnorm(sample_to)
par(mfrow = c(1,1))
#the resulting sample  normal. You might notice that the Q-Q plot
#looks less normal, bisut remember that the bottom row has 1/100 of the
#data of the top row.

#Let's wrap this into a function
ressamp = function(in_data, k, plotit=TRUE){
  sample_to = rep(NA, k)
  N = length(in_data)
  for(i in 1:k)
    sample_to[i] = in_data[i]
  
  for(i in (k+1):N){
    r = sample(1:i, 1)
    if(r <= k) 
      sample_to[r] = in_data[i]
  }
  if(plotit){
    par(mfrow = c(2,1))
    hist(in_data, main = 'Hist of Incoming Data')
    hist(sample_to, main='Hist of Our Sample')
  }
  sample_to #return what's on this line
}
new_rand = ressamp(rnorm(1e5), 1e2)
#Normal data goes in. Normal out.
new_unif = ressamp(runif(1e5), 1e2)
#Uniform in, uniform out

#Like I said, reservoir sampling actually gets a uniform sample of 
#whatever data is coming in. Magic! If you feel like doing the math, it works out.