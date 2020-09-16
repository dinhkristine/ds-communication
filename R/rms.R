rms = function(x,y,h){

# given covariate x, response y, and bandwidth h, compute running
# mean values for y over a sequence of 50 points over the range of x.

n = length(y)
ys = 0 # storage for predicted y values
# x values over which to make predictions in running mean
xs = seq(min(x), max(x), length=50)
for(i in 1:length(xs)){
  ys[i] = mean(y[(x>xs[i]-h)&(x<xs[i]+h)]) # mean y in neighborhood
}
list(y=ys, x=xs)
}