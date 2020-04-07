library(MASS)
library(invgamma)

Y <- Pima.tr$bp[Pima.tr$type == "No"]
X <- Pima.tr$bmi[Pima.tr$type == "No"]

plot(X,Y, pch = 19)

fr <- lm(Y~X)

m0 <- m1 <- 0
v0 <- v1 <- 100
a <- b <- 1

beta0 <- numeric()
beta1 <- numeric()
sigma2 <- numeric()

# starting values
beta0[1] <- 0
beta1[1] <- 0
sigma2[1] <- 1

n <- length(Y)
y_bar <- mean(Y)
y_var <- var(Y)

J <- 501000
for (j in 2:J) {
  
  # update sigma2
  a_star <- n/2 + a
  b_star <- .5*sum((Y - (beta0[j-1] + beta1[j-1]*X))**2) + b
  sigma2[j] <- rinvgamma(1,shape=a_star,rate=b_star)
  
  # update beta0
  v_star <- 1 / (n/sigma2[j] + 1/v0)
  m_star <- v_star * ((1/sigma2[j] * sum(Y - beta1[j-1]*X) + m0/v0))
  beta0[j] <- rnorm(1,mean=m_star,sd=sqrt(v_star))
  
  # Update beta1
  v_star <- 1/(sum(X**2)/sigma2[j] + 1/v1)
  m_star <- v_star *((1/sigma2[j])*sum(X*(Y - beta0[j])) + m1/v1)
  beta1[j] <- rnorm(1, mean = m_star, sd = sqrt(v_star))
}

keep <- seq(from=1001, to=J,by=50)

plot(beta0[keep],type='l')
plot(beta1[keep],type='l')
plot(sigma2[keep],type='l')


acf(beta0[keep])
acf(beta1[keep])
acf(sigma2[keep])

mean(beta1[keep])

mnb0 <- mean(beta0[keep])
mnb1 <- mean(beta1[keep])

xvals <- seq(min(X), max(X), length = 100)
yhat <- mnb0 + mnb1*xvals

plot(X, Y, pch = 20, cex=2)
lines(xvals,yhat,col='red')
abline(fr,col='blue')

ci_b0 <- quantile(beta0[keep], c(0.025,0.975))
ci_b1 <- quantile(beta1[keep], c(0.025,0.975))

yhat_L <- ci_b0[1] + ci_b1[1]*xvals
yhat_U <- ci_b0[2] + ci_b1[2]*xvals

reglines <- matrix(NA, nrow = length(keep), ncol = length(xvals))
for (jj in 1:length(xvals)) {
  
  reglines[jj,] <- beta0[keep][jj] + beta1[keep][jj]*xvals
  
}

plot(X,Y,pch=20,cex=2)
for (jj in 1:length(keep)) {
  
}

# Plot approach 2
mn_line <- apply(reglines,2,mean)
ci_line <- apply(reglines,2,function(x) quantile(x, c(0.025,0.975)))
plot(X,Y)
lines(xvals, mn_line, col='red')
lines(xvals, ci_lines, col = 'red', lty=3)