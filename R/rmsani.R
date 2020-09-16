rmsani = function(x,y,h,i){

# running mean smooth function used for animation.
# given covariate x, response y, and bandwidth h, compute running
# mean values for y over a sequence of 50 points over the range of x.
# i provides single "frame" for one (xi,yi) pair

n = length(y)
ys = 0
# sequence of x values over which to compute running mean
xs = seq(min(x), max(x), length=50)
interval = ani.options("interval")
while (i <= ani.options("nmax")) {
	# mean y in neighborhood
	ys[i] = mean(y[(x>xs[i]-h)&(x<xs[i]+h)])
	# scatterplot
      plot(x, y, xlab="Hardness", ylab="Mortality")
      #text(xs[1:i], ys[1:i], "X")
	lines(xs[1:i],ys[1:i])
	#text(x[(x>xs[i]-h)&(x<xs[i]+h)],y[(x>xs[i]-h)&(x<xs[i]+h)],"X",col="blue")
	lines(x[(x>xs[i]-h)&(x<xs[i]+h)], y[(x>xs[i]-h)&(x<xs[i]+h)], 
		type="p", pch=19, col="red")
	# draw vertical lines around neighborhood
	#abline(v=xs[i]-h); abline(v=xs[i]+h)
	# draw grey box around neighborhood
	lines(x=c(xs[i]-h-2,xs[i]-h-2,xs[i]+h+2,xs[i]+h+2), 
		y=c(min(y)-10,max(y)+10,max(y)+10,min(y)-10),
		lwd=3,col="grey")
	lines(x=c(xs[i]-h-2,xs[i]-h-2,xs[i]+h+2,xs[i]+h+2), 
		y=c(max(y)+10,min(y)-10,min(y)-10,max(y)+10),
		lwd=3,col="grey")
	text(xs[i],ys[i],"X")
	i=i+1
	Sys.sleep(interval)
    }
    invisible(NULL)
    data.frame(xs,ys)
}
