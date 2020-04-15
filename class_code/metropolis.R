g <- function(x) {  exp(-x**6)*(1+abs(2*x))**3 }

J <- 1000000
x <- numeric()
x[1] <- 1

tau <- 0.5 # proposal sd

for (j in 2:J) {
  xp <- rnorm(1, mean = x[j-1], sd = tau)
  
  gxp <- g(xp)
  gxc <- g(x[j-1])

  # Acceptance probability
  alpha <- min(1, gxp/gxc)
  
  U <- runif(1,0,1)
  
  x[j] <- ifelse(U < alpha, xp, x[j-1])
}

# Acceptance rate should be between 35% and 55%
mean(diff(x) != 0)

# thin the x's
plot(acf(x))
keep <- seq(1,J,by=20)
plot(acf(x[keep]))

plot(density(x[keep]))

# Integrating g
int_g <- integrate(g,-Inf,Inf)
plot(xx <- seq(-1.5,1.5,length=1000), g(xx) / int_g$value, col = 'red', lwd=3,type='l')
lines(density(x[keep]))

# estimates of posterior f(x)
mean(x[keep])
sd(x[keep])


## Part 2 
setwd("~/BYU/BYU Winter 2020/bayes-251/class_code")
df <- read.table("covid.txt", stringsAsFactors = F, header = T)
summary(df$Confirmed); hist(table(df$Confirmed));
summary(df$Deaths); hist(table(df$Deaths));

# Using Poisson Likelihood y_i | theta ~ Pois(theta)
# Use a uniform prior for theta ~ Uniform(0,b) 

# goal: Use metropolis algorithm to sample from the posterior

# pi(theta | y1 ... yn) Proportional to prod_{i=1}^n Pois(y_i | theta) Uniform(theta | 0,b)

# g(theta) = exp(-n * theta) * theta ^ sum(y_is) * I[0 < theta < b]
# log_g(theta) = -n*theta + sum(y_is) * log(theta) * I[0 < theta < b]

y <- df$Confirmed
n <- length(y)
y_sum <- sum(y)
# upper bound on theta
b <- 1000

theta <- numeric()
theta[1] <- mean(y)

J <- 10000
tau <- .5
log_g <- function(theta,n,sum_y) {-n * theta + sum_y * log(theta) }

for (i in 2:J) {
  tc <- theta[i-1]
  tp <- rnorm(1, tc, tau)
  
  # set next theta, replace if accepted new proposed theta
  theta[i] <- tc
  
  if (tp > 0 & tp < b) {
    gp <- log_g(tp, n, y_sum)
    gc <- log_g(tc, n ,y_sum)
    
    # Acceptance probability
    alpha <- min(1,exp(gp - gc))
    
    U <- runif(1, 0, 1)
    # 
    if (alpha > U) {
      theta[i] <- tp
    }
  }
}

# acceptance ratio
mean(diff(theta) != 0)

plot(theta, type='l')

keep <- seq(1,J,by=10)
plot(theta[keep])
acf(theta[keep])

quantile(theta[keep], c(0.025,0.975))






