pathdistance = function(path, distances){
  stopifnot(is.matrix(distances) & min(distances) == 0)
  #first find the lenght of the first path
  m = length(path) - 1
  pd = distances[path[1], path[2]] #distances is a matrix. 
  if(m+1 > 2) for(i in 2:m){
    #assuming we have more then 2 cities
    #we then add the distances for additional paths
    pd = pd + distances[path[i], path[i+1]]
  }
  pd #return pd
}
switch_nonconsecutive = function(path){ #a potential proposal distribution
  #exchange 2 non-consecutive nodes on the path.
  n = length(path)
  stopifnot(n >= 4)
  #choose 2 cities randomly, then switch them
  inidices = sample(2:(n-1), 2, replace=FALSE)
  #the next 3 lines just switch the 2 cities. 
  temp = path[inidices[1]]
  path[inidices[1]] = path[inidices[2]]
  path[inidices[2]] = temp
  return(path)
}
vispath = function(path, cities, title='Traveling Salesman Path'){
  #function to visualize a path. 
  #This one is a lot more pretty and detailed than what's required in your homework. 
  #If you prefer to use the base graphics, no problem.
  library(ggplot2)
  locations = as.data.frame(cities[path,]) #put the x,y coordinates in the order of the path
  ggplot(locations, aes(x=x, y=y)) + geom_path() + geom_point(col='red', size=3) + 
    theme_minimal() + ggtitle(title) + annotate('point', x = locations[1,1], y = locations[1,2], color='blue', size=7)

    #How would you do this in base graphics? Try the following 4 lines
    #plot(locations, main=title)
    #lines(locations)
    #points(1,0, col='red', pch=16)
    #points(locations[1,], col='red', pch=16)
}
placeCitiesCircle = function(m){
  #create a vector of potential values for theta. 
	#without [1:(m+1)] at the end, the last value will be the same as the first...
	#so we're only want the first m+1 values.
	radius=1
	thetas = seq(0, 2*pi, by = 2*pi/(m+1))[1:(m+1)]
	#translate to x,y values, put into a matrix
	cities = cbind( 
		x = radius*cos(thetas),
		y = radius*sin(thetas)
		)
	cities #return this
}
MetropolisWalkCircle = function(m, T, Nsim=1e4, radius=1, plotbest=TRUE){
	#get locations of m+1 cities for our given m
	cities = placeCitiesCircle(m)
	distances = as.matrix(dist(cities))
	#set the initial path for U[1]
	#path starts at 1, then goes to the rest randomly
	path = c(1, sample(2:(m+1), replace=FALSE), 1) 
	#creat a vector for U, we'll write over all but U[1]
	U = rep(pathdistance(path, distances), Nsim)
	#remember which path is best. 
	bestpath = path 
	for(t in 2:Nsim){
		newpath = switch_nonconsecutive(path)
		Uold = U[t-1]
		Unew = pathdistance(newpath, distances)
		prob = ifelse(Unew < Uold, 1, exp(-(Unew - Uold)/T))
		if(runif(1) < prob){
		  path = newpath
		  U[t] = Unew
		} else U[t] = Uold

		if(U[t] <= min(U)) 
		  bestpath = path
	}
	if(plotbest) 
		plot(vispath(bestpath, cities, paste0('T = ', T, ', Length = ', round(min(U),2))))
	#if the function doesnt see return() somewhere first, it will return whatever's on the last line.
	list(U = U, bestpath = bestpath)
}
SimAnnealWalkCircle = function(m, T0, rate, Nsim=1e4, radius=1, plotbest=TRUE){
	#get locations of m+1 cities for our given m
	cities = placeCitiesCircle(m)
	distances = as.matrix(dist(cities))
	#set the initial path for U[1]
	#path starts at 1, then goes to the rest randomly
	path = c(1, sample(2:(m+1), replace=FALSE), 1) 
	#create a vector for U, we'll write over all but U[1]
	U = rep(pathdistance(path, distances), Nsim)
	#remember which path is best.
	bestpath = path 
	T = T0
	for(t in 2:Nsim){
	  newpath = switch_nonconsecutive(path)
		Uold = U[t-1]
		Unew = pathdistance(newpath, distances)
		prob = ifelse(Unew < Uold, 1, exp(-(Unew - Uold)/T))
		if(runif(1) < prob){
		  path = newpath
		  U[t] = Unew
		} else U[t] = Uold

		if(U[t] <= min(U)) bestpath = path

		T = T * (1 - rate)
	}
	if(plotbest) 
		plot(vispath(bestpath, cities, paste0('T0 = ', T0, ', Rate = ', signif(rate, 1), ', Length = ', round(min(U),2))))
	#if the function doesnt see return() somewhere first, it will return whatever's on the last line.
	list(U = U, bestpath = bestpath)
}
placeCitiesSquare = function(m){
	#first value is at 0,0, others are in a unit box
	x = c(0, runif(m))
	y = c(0, runif(m))
	cbind(x,y) #this last line gets returned
}
SimAnnealWalkSquare = function(m, T0, rate, Nsim=1e4, radius=1, plotbest=TRUE){
	#get locations of m+1 cities for our given m
	cities = placeCitiesSquare(m)
	distances = as.matrix(dist(cities))

	#set the initial path for U[1]
	#path starts at 1, then goes to the rest randomly
	path = c(1, sample(2:(m+1), replace=FALSE), 1) 
	#creat a vector for U, we'll write over all but U[1]
	U = rep(pathdistance(path, distances), Nsim)
	#remember which path is best.
	bestpath = path 
	T = T0
	for(t in 2:Nsim){
	  newpath = switch_nonconsecutive(path)
		Uold = U[t-1]
		Unew = pathdistance(newpath, distances)
		prob = ifelse(Unew < Uold, 1, exp(-(Unew - Uold)/T))
		if(runif(1) < prob){
		  path = newpath
		  U[t] = Unew
		} else U[t] = Uold

		if(U[t] <= min(U)) bestpath = path

		T = T * (1 - rate)
	}
	if(plotbest) 
		plot(vispath(bestpath, cities, paste0('T0 = ', T0, ', Rate = ', signif(rate, 1), ', Length = ', round(min(U),2))))
	#if the function doesnt see return() somewhere first, it will return whatever's on the last line.
	list(U = U, bestpath = bestpath)
}
