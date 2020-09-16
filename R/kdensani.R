kdensani = function(x,kdest,i){
# Kernel density estimate with N(0,1) kernels

n = length(x)
xs = seq(0, 12, 0.1)
kdesta = 0
interval = ani.options("interval")
while (i <= ani.options("nmax")) {
	kdesta[i] = sum(dnorm(xs[i], x, sd = 1))
	# set up axes
	plot(xs, kdest, type="n", xlab="x", ylab="Density")
	# plot data values (with an 'X')
	text(x,0,"X")
	abline(h=0, lty=3)
	# plot normal densities over each observation (in red)
	usr=par("usr")
	par(usr=c(usr[1:2],-0.03,1))
	for(j in 1:length(x)){
		xsn = seq(x[j]-3,x[j]+3,0.1)
		lines(xsn, dnorm(xsn,x[j],1), pch=19, lty=2, col="red")
	}
	# draw vertical gray line at xs value over which estimating density
	abline(v=xs[i],lwd=3,col="grey")
	# present running computation of kernel density estimate as scanning over xs
	lines(xs[1:i],kdesta[1:i])
	i=i+1
	Sys.sleep(interval)
    }
    invisible(NULL)
    #data.frame(xs,kdesta)
}
