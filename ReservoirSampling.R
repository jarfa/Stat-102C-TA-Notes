#Reservoir Sampling
set.seed(50)
N = 1e5
#sample_from = 1:N
sample_from = rnorm(N)
k = 1e3
sample_to = rep(NA, k)
for(i in 1:N){
  if(i<=k) {
    sample_to[i] = sample_from[i]
  } else{
    r = sample(1:i, 1)
    if(r <= k) 
      sample_to[r] = sample_from[i]
  } 
}
sum(is.na(sample_to))
hist(sample_to)
qqnorm(sample_to)