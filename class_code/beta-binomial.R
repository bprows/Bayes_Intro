## Beta Binomial Example
# Plot the binomial likelihood for n=10 and ??=0.9. Make sure
# that it is properly formatted to be a self-explanatory graphic.
par(mfrow=c(1,2))
X <- 0:10
fX <- dbinom(X, size = 10, prob = 0.9)
plot(X, fX, type = 'h', xlab = 'X', ylab = 'Density', main = "X~Binom(n=10,p=0.9)")
# Plot the binomial likelihood for n=10 and ??=0.85.
X <- 0:10
fX <- dbinom(X, size = 10, prob = 0.85)
plot(X, fX, type = 'h', xlab = 'X', ylab = 'Density', main = "X~Binom(n=10,p=0.85)")
par(mfrow=c(1,1))
# If n=10 and y= 7, which value of ??, 0.9 or 0.85, leads to a
# higher likelihood (i.e., a higher value of f (y|??))?
# 0.85

# Plotting pdf of beta distributions
# Beta(1,1)
par(mfrow=c(3,2))
X <-seq(0,1,length.out = 1000)
fX <- dbeta(X, shape1=1, shape2=1)
plot(X, fX, type = 'l', lwd = 2, xlab = expression(theta), ylab = 'Density', main = "Beta(1,1)")

# Beta(3,7)
fX <- dbeta(X, shape1=3, shape2=7)
plot(X, fX, type = 'l', lwd = 2, xlab = expression(theta), ylab = 'Density', main = "Beta(3,7)")

# Beta(100, 74)
fX <- dbeta(X, shape1=100, shape2=74)
plot(X, fX, type = 'l', lwd = 2, xlab = expression(theta), ylab = 'Density', main = "Beta(100,74)")

# Beta(0.2, 5)
fX <- dbeta(X, shape1=.2, shape2=5)
plot(X, fX, type = 'l', lwd = 2, xlab = expression(theta), ylab = 'Density', main = "Beta(0.2,5)")

# Beta(0.5, 0.5)
fX <- dbeta(X, shape1=.5, shape2=.5)
plot(X, fX, type = 'l', lwd = 2, xlab = expression(theta), ylab = 'Density', main = "Beta(.5,.5)")

par(mfrow=c(1,1))
plot(X,dbeta(X,shape1 = 100, shape2 = 74), type = 'l', lwd=2, 
     xlab = expression(theta), ylab = "Density", main = "Beta")
lines(X,dbeta(X,shape1=1,shape2=1), type = 'l', lwd = 2, col = 'blue')
lines(X,dbeta(X,shape1=3,shape2=7), type = 'l', lwd = 2, col = 'red')

#### Example ####
# Theta = proportion of US transportation workers under the influence. 
# Prior: theta ~ Beta(1.4, 23.6)
X <- seq(0,1,length.out=1001)
a <- 1.4
b <- 23.6
pi_theta <- dbeta(X,shape1=a,shape2=b)
plot(X,pi_theta,
     main = "Proportion of US Transportation workers under the influence",
     xlab = expression(theta),
     ylab = 'density',
     type = 'l', lwd = 2)

# E(theta): a / (a+b)
a / (a+b)
# Prior Mode: a-1 / a+b-2
(a-1) / (a+b-2)
# prior Interval
qbeta(c(0.0275,0.975),shape1 = a, shape2 = b)

# collect Data: n = 12 x = 3
a_star <- a+3
b_star <- b + 9
post_theta <- dbeta(X,shape1 = a_star, shape2 = b_star)
lines(X,post_theta, type = 'l', lwd = 2, col = 'red')
# E(theta): a / (a+b)
a_star / (a_star+b_star)
# Prior Mode: a-1 / a+b-2
(a_star-1) / (a_star+b_star-2)
# prior Interval
qbeta(c(0.0275,0.975),shape1 = a_star, shape2 = b_star)


