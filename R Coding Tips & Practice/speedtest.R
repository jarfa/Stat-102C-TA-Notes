#There are three ways to add numbers to a vector inside a for loop:
#1) vector = c(vector, value)
#2) vector[i] = value, where we've already told R how long the vector is going to be
#3) vector[i] = value, but we're expanding the length of the vector at each step.

#Which is fastest?
concat = function(N){ 
  #function were we concatenate 2 new vectors together at each step
  num = numeric()
  for(i in 1:N)
    num = c(num, i)
  num
}
alloc = function(N){ 
  #function where we allocate an array first
  num = numeric(N)
  for(i in 1:N) num[i] = i
  num
}
no_alloc = function(N){
  #no spaces is allocated, but we're assigning a value to num[i] anyway
  num = numeric()
  for(i in 1:N)
    num[i] = i
  num
}

outlength=10
timedata = data.frame(
  N = seq(1e2, 5e4, length.out = outlength),
  alloc = numeric(outlength),
  concat = numeric(outlength),
  no_alloc = numeric(outlength)
  )

for(i in 1:outlength){
  timedata$alloc[i] = system.time(alloc(timedata$N[i]))[3]
  timedata$concat[i] = system.time(concat(timedata$N[i]))[3]
  timedata$no_alloc[i] = system.time(no_alloc(timedata$N[i]))[3]
}

library(ggplot2)
library(reshape2)
timedata1 = melt(timedata, id.vars = 'N', variable.name = 'type', value.name = 'time')

ggplot(timedata1, aes(x=N, y=time, color = type)) + geom_line(size=2) + 
  theme_minimal()

ggplot(timedata1, aes(x=N, y=time, color = type)) + geom_line(size=2) + 
  theme_minimal() + scale_y_sqrt()

#for extra credit: make plots demonstrating that pre-allocating a vector
#of length N leads to an O(n) operation, while the other two are O(n^2)