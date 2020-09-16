# Illustration on the water hardness data
load("../Data/water.Rdata")
attach(water)
n = length(mortality)
# start with automatic bandwidth obtained from Wand & Jones kernel smooth
library(KernSmooth)
h = dpill(hardness,mortality)
# animation package for the illustrations
library(animation)

# Running mean smooth illustration
source("rms.R") # running mean smooth function
# plot the running mean smooth over the scatterplot
# note "choppy" and "biased" on edges (flattens out near endpts)
plot(hardness,mortality)
lines(rms(hardness,mortality,h))
rmssmooth = rms(hardness,mortality,h)

# animation of running mean smooth point-by-point
# Code taken from AniWIKI http://animation.yihui.name/animation:start
source("rmsani.R")
oopt = ani.options(interval = 0.25, nmax = 50)
#brownian.motion(n = 15, pch = 21, cex = 5, col = "red", bg = "yellow")
# function creating smooth in each neighborhood of 50 equally-spaced points
rmsfit = rmsani(hardness,mortality,h,1)
ani.options(oopt)
plot(hardness, mortality, 
	xlab="Hardness", ylab="Mortality", main="Running line smooth")
lines(rmsfit$xs, rmsfit$ys)

# show one point
source("rmsani.R")
loc=28
oopt = ani.options(interval = 0.5, nmax = loc)
#brownian.motion(n = 15, pch = 21, cex = 5, col = "red", bg = "yellow")
rmsani(hardness,mortality,h,loc)
ani.options(oopt)


# Running line smooth illustration
source("rlsani.R")
oopt = ani.options(interval = 0.25, nmax = 50)
rls=rlsani(hardness,mortality,h,1)
ani.options(oopt)
plot(hardness, mortality, 
	xlab="Hardness", ylab="Mortality", main="Running line smooth")
lines(rls$xs, rls$ys)
source("rms.R")
lines(rms(hardness,mortality,h), lty=2)
# for large bandwidth, bias and flattening-out of trends near border more apparent

# show one point
source("rlsani.R")
loc=28
oopt = ani.options(interval = 0.5, nmax = loc)
rlsani(hardness,mortality,h,loc)
ani.options(oopt)


# Locally-weighted running line smooth illustration
windows()
h = 30
source("lwrlsani.R")
oopt = ani.options(interval = 0.25, nmax = 50)
lwrl=lwrlsani(hardness,mortality,h,1)
ani.options(oopt)
plot(hardness, mortality, 
	xlab="Hardness", ylab="Mortality", 
	main="Locally-weighted running line smooth; tri-cube weight")
lines(lwrl$xs, lwrl$ys)
#dev.set(2)
#lines(lwrl$xs, lwrl$ys)
source("rms.R")
lines(rms(hardness,mortality,h), lty=2)

# show one point
source("lwrlsani.R")
loc=28
h=30
oopt = ani.options(interval = 0.5, nmax = loc)
lwrlsani(hardness,mortality,h,loc)
ani.options(oopt)
