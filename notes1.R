# Beta distribution
# f(x) = 12(x**2)(1-x) for 0 < x < 1
Xs <- seq(0,1,length.out = 1001)
f.Xs <- 12 * (Xs**2) * (1-Xs)
plot(Xs, f.Xs, type = 'l')

# Standard Normal
# f(x) = 1 / sqrt(2pi)exp(-x**2/2)
Xs <- seq(-4,4,length.out=1001)
f.Xs <- (1 / sqrt(2*pi)) * exp( (-Xs**2) / 2 )
plot(Xs, f.Xs, type = 'l')

