SP <- read.csv("DSP500.csv", header = TRUE, sep = ",")
names(SP)
str(SP)
summary(SP)
DSP <- SP$Close
DSP_time <- seq(from=1960, to=2019, length.out = length(DSP))
DSPLR <- diff(log(DSP))
par(mfrow = c(1,2), cex = 0.8)
plot(DSP_time, DSP, type="l", lwd=2, xlab = "Year", main = "Daily S&P500 index from January 1960 to July 2019")
plot(DSP_time[2:length(DSP)], DSPLR, type = "l", lwd=2, xlab = "Year", main = "Daily log returns of S&P500 from Jan 1960 to July 2019")
par(mfrow=c(1,2))
qqnorm(DSPLR, main = "Normal QQ plot of DSPLR")
hist(DSPLR, freq = FALSE, main = "Histogram of DSPLR")
par(mfrow=c(1,1))
hist(DSPLR, breaks = 100, freq = FALSE, main = "Histogram
     of DSPLR vs Fitted Normal Density")
mu_DSPLR <- mean(DSPLR)
sd_DSPLR <- sd(DSPLR)
curve(dnorm(x, mean = mu_DSPLR, sd = sd_DSPLR), col="red", lwd = 2, add=TRUE)
hist(DSPLR, breaks=100, freq=FALSE, main = "Histogram of DSPLR vs Fitted t density", ylim = c(0,60))
library(MASS)      
DSPLR_t <- fitdistr(DSPLR, "t")
m <- DSPLR_t$estimate[1]
lambda <- DSPLR_t$estimate[2]
nu <- DSPLR_t$estimate[3]
curve(dt((x-m)/lambda,df=nu)/lambda, col ="red", lwd=2, add=TRUE)
kd <- density(DSPLR, kernel = "gaussian", bw=0.001)
hist(DSPLR, breaks=100, freq=FALSE, main="Histogram and KDE of DSPLR", ylim = c(0,65))
points(kd, type="l", lwd=2, col="red")
n <- length(DSPLR)
q <- seq(1/(n+1), n/(n+1), by=1/n)
q_norm <- qnorm(q, mean=mu_DSPLR, sd=sd_DSPLR)
par(mfrow=c(1,2))
qqplot(q_norm, DSPLR, main="QQ Plot of DSPL vs Fitted normal", xlab="Fitted normal quantiles", ylab ="DSPLR quantiles")
abline(0,1, col="red")
q_t <- m+lambda*qt(q,df=nu)
qqplot(q_t, DSPLR, main="QQ Plot of DSPLR vs Fitted t", xxlab = "Fitted t quantiles", ylab = "DSPLR quantiles")
abline(0,1,col="red")
q <- 0.01
var_emp <- -quantile(DSPLR, q)
var_emp
var_normal <- -qnorm(q, mu_DSPLR, sd_DSPLR)
var_normal
var_t <- -(m + lambda*qt(q,df=nu))
var_t
c(var_emp, var_normal, var_t)
