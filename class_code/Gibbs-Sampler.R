# Gibbs Sampling 
library(invgamma)
library(MASS)
## Y-values of data
ndb <- Pima.tr$bp[Pima.tr$type == "No"]

## EDA
hist(ndb)
summary(ndb)

## Prior values
m <- 80  ## prior mean for mu
v <- 9   ## prior variance for mu
a <- 5   ## shape of prior for sigma2
b <- 400 ## rate of prior for sigma2
n <- length(ndb)
y_bar <- mean(ndb)

## vectors to hold iterates 
mu <- numeric()
sig2 <- numeric()

## insert starting values 
mu[1] <- y_bar
sig2[1] <- var(ndb)

## number of samples
J <- 10000
for (j in 2:J) {
  
  # update mu
  m_star <- (n*v*y_bar + sig2[j-1]*m) / (n*v + sig2[j-1])
  v_star <- (v*sig2[j-1]) / (n*v + sig2[j-1])
  mu[j] <- rnorm(1,mean = m_star, sd = sqrt(v_star))
  
  # update sig2
  a_star <- a + n/2
  b_star <- b + sum((ndb-mu[j])**2)/2
  sig2[j] <- rinvgamma(1, shape = a_star, rate = b_star)
  
}

plot(mu, type = 'l')
plot(density(mu))

plot(sig2,type= 'l')
plot(density(sig2))
x<-seq(0,200,length.out=1001)
lines(x,dinvgamma(x,shape=a,rate=b),col='red')


gibbs_nnig <- function(m,v,a,b,data,J=10000) {
  ## set up parameters from data
  n <- length(data)
  y_bar <- mean(data)
  
  ## vectors to hold iterates 
  mu <- numeric()
  sig2 <- numeric()
  
  ## insert starting values 
  mu[1] <- y_bar
  sig2[1] <- var(ndb)
  
  for (j in 2:J) {
    
    # update mu
    m_star <- (n*v*y_bar + sig2[j-1]*m) / (n*v + sig2[j-1])
    v_star <- (v*sig2[j-1]) / (n*v + sig2[j-1])
    mu[j] <- rnorm(1,mean = m_star, sd = sqrt(v_star))
    
    # update sig2
    a_star <- a + n/2
    b_star <- b + sum((ndb-mu[j])**2)/2
    sig2[j] <- rinvgamma(1, shape = a_star, rate = b_star)
    
  }
  
  output <- list(mu,sig2)
  return(output)
  
}
18008648331



