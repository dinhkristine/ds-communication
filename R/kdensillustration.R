x = c(2.5, 4.2, 4.8, 8, 9)
xs = seq(0, 12, 0.1)
dnorm(x, mean = 0, sd = 1, log = FALSE)
# kernel density estimate with Gaussian kernels
kdest = 0
s = 1
for(i in 1:length(xs)){
	# for "bandwidth" illustration, can change sd to 0.5 and 2
	kdest[i] = sum(dnorm(xs[i], x, sd = s))
}
plot(xs, kdest, type="n", xlab="x", ylab="Density", ylim=c(0,1))
lines(xs,kdest)
text(x,0,"X")
abline(h=0, lty=3)
# plot normal densities over each observation
usr=par("usr")
par(usr=c(usr[1:2],-0.03,1))
for(i in 1:length(x)){
	xsn = seq(x[i]-3,x[i]+3,0.1)
	lines(xsn, dnorm(xsn,x[i],s), pch=19, lty=2, col="red")
}


# animation of running mean smooth point-by-point
# Code taken from AniWIKI http://animation.yihui.name/animation:start
# Compute kernel density to fix plot axes at right limits
library(animation)
kdest = 0
for(i in 1:length(xs)){
	kdest[i] = sum(dnorm(xs[i], x, sd = 1))
}
source("kdensani.R")
oopt = ani.options(interval = 0.05, nmax = length(xs))
kdensani(x,kdest,1)
ani.options(oopt)
# final plot after animation concludes
plot(xs, kdest, type="n", xlab="x", ylab="Density")
lines(xs,kdest)
text(x,0,"X")
abline(h=0, lty=3)
# plot normal densities over each observation
usr=par("usr")
par(usr=c(usr[1:2],-0.03,1))
for(i in 1:length(x)){
	xsn = seq(x[i]-3,x[i]+3,0.1)
	lines(xsn, dnorm(xsn,x[i],1), pch=19, lty=2, col="red")
}

#oopt = ani.options(interval = 0.05, nmax = length(xs))
#saveMovie(kdensani(x,kdest,1), interval = 0.05, moviename="kdtry")


# show one point
kdest = 0
for(i in 1:length(xs)){
	kdest[i] = sum(dnorm(xs[i], x, sd = 1))
}
source("kdensani.R")
loc = 28
oopt = ani.options(interval = 0.05, nmax = loc)
kdensani(x,kdest,loc)
ani.options(oopt)
lines(xs[loc], kdest[loc], type="p")
lines(xs,kdest)


