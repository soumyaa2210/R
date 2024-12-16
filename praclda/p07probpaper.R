#question1 mandatory for everyone
x1<-qnorm(seq(0.01,0.99,0.01))
x2<-qnorm(seq(0.01,0.99,0.01))
plot(x1,x2,type="n")
abline(h=x1)
abline(v=x1)
n=300
s1<-sort(rnorm(n))
tq<-qnorm(seq(0.01,0.99,length=n))
points(s1,tq,col=2,pch=16)

#q2exp distn
x1<-qexp(seq(0.01,0.99,0.01))
x2<-qexp(seq(0.01,0.99,0.01))
plot(x1,x2,type="n")
abline(h=x1)
abline(v=x1)
n=300
s1<-sort(rexp(n))
tq<-qexp(seq(0.01,0.99,length=n))
points(s1,tq,col=2,pch=16)

# Lognormal Distribution
x1 <- qlnorm(seq(0.01, 0.99, 0.01), meanlog = 0, sdlog = 1)  # Theoretical Lognormal quantiles
x2 <- qlnorm(seq(0.01, 0.99, 0.01), meanlog = 0, sdlog = 1)
plot(x1, x2, type = "n", main = "Lognormal Q-Q Plot")
abline(h = x1)
abline(v = x1)
n <- 300
s1 <- sort(rlnorm(n, meanlog = 0, sdlog = 1))  # Sample Lognormal data
theoretical_quantile <- qlnorm(seq(0.01, 0.99, length = n), meanlog = 0, sdlog = 1)
points(s1, theoretical_quantile, col = 2, pch = 16)

# Weibull Distribution
x1 <- qweibull(seq(0.01, 0.99, 0.01), shape = 2)  # Theoretical Weibull quantiles with shape parameter 2
x2 <- qweibull(seq(0.01, 0.99, 0.01), shape = 2)
plot(x1, x2, type = "n", main = "Weibull Q-Q Plot")
abline(h = x1)
abline(v = x1)
n <- 300
s1 <- sort(rweibull(n, shape = 2)) #sample points
theoretical_quantile <- qweibull(seq(0.01, 0.99, length = n), shape = 2)
points(s1, theoretical_quantile, col = 2, pch = 16)

# Gamma Distribution
x1 <- qgamma(seq(0.01, 0.99, 0.01), shape = 2)  # Theoretical Gamma quantiles with shape parameter 2
x2 <- qgamma(seq(0.01, 0.99, 0.01), shape = 2)
plot(x1, x2, type = "n", main = "Gamma Q-Q Plot")
abline(h = x1)
abline(v = x1)
n <- 300
s1 <- sort(rgamma(n, shape = 2))  # Sample Gamma data with shape parameter 2
theoretical_quantile <- qgamma(seq(0.01, 0.99, length = n), shape = 2)
points(s1, theoretical_quantile, col = 2, pch = 16)


