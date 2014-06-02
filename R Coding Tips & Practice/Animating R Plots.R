#Sample code for creating animated plots and gifs.
library(animation)
testfun = function(N){
  colorrange = rainbow(N) #a vector of color codes
  for(i in 1:N){
    #these are just sample plots
    plot(-1:1, -1:1, type='n', xlab='x',  ylab='y')
    points(rnorm(1e3, sd=0.25), rnorm(1e3, sd=0.25), col=colorrange[i], pch=16)
    text(-0.9, 0.9, label=i, cex=2)
    ani.pause()
  }
}
#interval = how long each frame is held
ani.options(interval = 0.1)
testfun(20)
#what's key is that whatever's inside saveGIF() must display a series
#of plots, each sepearted by ani.pause(). It should output an animated gif
saveGIF(testfun(20))