lwrlsani = function(x,y,h,i){

# locally-weighted running line smooth for animation

n = length(y)
ys = 0
xs = seq(min(x), max(x), length=50)
#i = 1
interval = ani.options("interval")
while (i <= ani.options("nmax")) {
	# identify y and x values in neighborhood
	ynew = y[(x>xs[i]-h)&(x<xs[i]+h)]
	xnew = x[(x>xs[i]-h)&(x<xs[i]+h)]
	# store value of x at which to make prediction in neighborhood
	xpred = data.frame(xnew = xs[i])
	# weight points in neighborhood
	#wt = dnorm(xnew,xs[i],1)
	# LOWESS tri-cube weight function (H&T pg. 30)
      wt = (1-(abs(xnew-xs[i])/max(abs(xnew-xs[i])))^3)^3
	ys[i] = predict(lm(ynew~xnew, weights=wt), newdata = xpred)
	# use whole data set
	#xpred = data.frame(x = xs[i])
	#wt = dnorm(x,xs[i],1)
	#ys[i] = predict(lm(y~x, weights=wt), newdata = xpred)
      plot(x, y, xlab="Hardness", ylab="Mortality")
	# plot regression line over whole range of x
	#abline(lm(y~x, weights=wt), col="blue")
	# plot regression line only in neighborhood
	x1 = c(xs[i]-h,xs[i]+h)
	xpred=data.frame(xnew=x1)
	y1=predict(lm(ynew~xnew, weights=wt), newdata = xpred)
	lines(x1,y1,col="blue")
	# plot smooth predicted values, as points "X", up to current xs
      ##text(xs[1:i], ys[1:i], "X")
	# plot smooth, as a line, up to current xs 
	lines(xs[1:i],ys[1:i])
	##text(x[(x>xs[i]-h)&(x<xs[i]+h)],y[(x>xs[i]-h)&(x<xs[i]+h)],"X",col="blue")
	lines(x[(x>xs[i]-h)&(x<xs[i]+h)], y[(x>xs[i]-h)&(x<xs[i]+h)], 
		type="p", pch=19, col="red")
	# draw thickish grey vertical lines around neighborhood
	abline(v=xs[i]-h, lwd=3, col="grey"); abline(v=xs[i]+h, lwd=3, col="grey")
	# draw grey box around neighborhood
	#lines(x=c(xs[i]-h-2,xs[i]-h-2,xs[i]+h+2,xs[i]+h+2), 
	#	y=c(min(y)-10,max(y)+10,max(y)+10,min(y)-10),
	#	lwd=3,col="grey")
	#lines(x=c(xs[i]-h-2,xs[i]-h-2,xs[i]+h+2,xs[i]+h+2), 
	#	y=c(max(y)+10,min(y)-10,min(y)-10,max(y)+10),
	#	lwd=3,col="grey")
	text(xs[i],ys[i],"X")
	# plot tri-cube function at bottom of plot in nbhd
	usr=par("usr")
	par(usr=c(usr[1:2],0, 10))
	lines(sort(xnew), (1-(abs(sort(xnew)-xs[i])/max(abs(xnew-xs[i])))^3)^3, pch=19)
	i=i+1
	Sys.sleep(interval)
    }
    invisible(NULL)
    data.frame(xs,ys)
}
