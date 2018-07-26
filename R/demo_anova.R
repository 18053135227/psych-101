library('data.table')
library('ggplot2')

rm(list = ls())

## NOTE: Begin testing H about variance
## 0. Observe data / perform an experiment
## draw n samples from X = whatever_generates_data_for_your_experiment
n <- 1000
muX <- 0
varX <- 15
sigmaX <- sqrt(varX)
x <- rnorm(n, muX, sigmaX)

## 1. state H's
## H0: sigmasq = 10
## H1: sigmasq > 10
varxH0 <- 10

## 2. choose confidence
alpha <- 0.05

## 3. choose a statistic to estimate the parameter in H0 and determine its
## sampling distribution. The obvious choice is the sample variance.
xvar <- var(x)

## NOTE: How is sigmasq_hat = var() distributed?
## Lets try to answer this question by estimating the sampling distribution of
## sigma_hat by performing a bunch of experiments and constructing a histogram.


## NOTE: If X ~ N(muX, sigmaX):
n <- c(2,5,10,20) ## choose sample sizes to explore
d_rec <- list()
for(j in n) {
  for(i in 1:5000) { ## run 5000 experiments... kind of a lot...
    x <- rnorm(j, muX, sigmaX)
    varx <- var(x) ## the test statistic of interest here is sample variance
    d_rec[[i + j*1000]] <- data.table(exp=i, n=j, varx) ## cool index trick here
  }
}

d <- rbindlist(d_rec)

## varx is definitely not Normally distributed
ggplot(d, aes(varx)) +
  geom_histogram(aes(y=..density..), bins=50) +
  geom_density(colour='red') +
  facet_wrap(~n, scale='free') +
  theme(aspect.ratio = 1)


## NOTE: Turns out, if X ~ N(muX,sigmaX), then a chi-squred distribution with
## n-1 degrees of freedom is a nice model for the sampling distribution of varx.

## Chisq(df) = (n-1) * sample_variance(x) / population_variance(X)
## with df = n -1

x <- seq(0.001, 10, 0.01)
fx1 <- dchisq(x, 1)
fx2 <- dchisq(x, 2)
fx3 <- dchisq(x, 3)
fx4 <- dchisq(x, 4)
fx5 <- dchisq(x, 5)
fx6 <- dchisq(x, 6)

d <- data.table(x, fx1, fx2, fx3, fx4, fx5, fx6)
dd <- melt(d, id.vars='x')

ggplot(dd, aes(x=x, y=value, colour=variable)) +
  geom_line() +
  ylim(0,0.5) +
  theme(aspect.ratio = 1)

## NOTE: Chisq(df) as a test statistic:
## This:
## Chisq(df) = (n-1) * sample_variance(x) / population_variance(X)
## df=n-1
## Becomes this:
## chisq_obs = (n-1) * sd(x) / varxH0
## df=n-1

## E.g.,
n <- 1000
muX <- 0
varX <- 15
sigmaX <- sqrt(varX)
x <- rnorm(n, muX, sigmaX)

## 1. state H's
## H0: sigma = 10
## H1: sigma > 10
varxH0 <- 10

## 2. choose confidence
alpha <- 0.05

## 3. choose a statistic to estimate the parameter in H0 and determine its
## sampling distribution.
## X ~ N(muX, sigmaX)
## X_obs ~ Chisq(df)
df <- n - 1
chisq_obs = df * var(x) / varxH0

## 4. compute p- and critical- values
p_obs <- pchisq(chisq_obs, df, lower.tail = FALSE)
chisq_crit <- qchisq(alpha, df, lower.tail = FALSE)

## 5. Make a decision
if(p_obs < alpha) {
  print('reject H0')
} else {
  print('fail to reject H0')
}

if(chisq_obs > chisq_crit) {
  print('reject H0')
} else {
  print('fail to reject H0')
}

## Or using R for a one liner
## install.packages('EnvStats')
library('EnvStats')
varTest(x, alternative='greater', sigma.squared=10)


## NOTE: The above test for variance is very sensitive to the assumption of
## Normality. We may or may not explore this in the homework, or even in class
## if time permits. If it appears on your homework, the preceding chunks of code
## will probably be quite useful to you.


## NOTE: Comparing the variance of two Normal populations
## 0. Observe data / perform an experiment
## draw n samples from X = whatever_generates_data_for_your_experiment
n <- 1000

muX <- 0
varX <- 5
sigmaX <- sqrt(varX)
x <- rnorm(n, muX, sigmaX)

muY <- 0
varY <- 5
sigmaY <- sqrt(varY)
y <- rnorm(n, muX, sigmaY)

## 1. state H's
## H0: sigmaX = sigmaY
## H1: sigmaX < sigmaY

## TODO: comment on the strangeness of this approach... however, it is
## nevertheless THE approach.
## H0: sigmaX/sigmaY = 1
## H1: sigmaX/sigmaY < 1
var_ratio_H0 <- 1

## 2. choose confidence
alpha <- 0.05

## 3. choose a statistic to estimate the parameter in H0 and determine its
## sampling distribution. Seems pretty intuitive to choose the ratio of the two
## sample variances.
var_ratio_obs <- var(x) / var(y)


## NOTE: What is the sampling distribution of var_ratio_obs?
## Agin, lets try to answer this question by estimating the sampling
## distribution of sigma_hat by performing a bunch of experiments and
## constructing a histogram.

## If X ~ N(muX, sigmaX):
n <- c(2,5,10,20) ## the sample sizes per experiment to explore
d_rec <- list()
for(j in n) {
  for(i in 1:5000) { ## the number of experiments to perform
    x <- rnorm(j, muX, sigmaX)
    y <- rnorm(j, muY, sigmaY)
    var_ratio <- var(x) / var(y)
    d_rec[[i + j*1000]] <- data.table(exp=i, n=j, var_ratio)
  }
}

d <- rbindlist(d_rec)

## var_ratio = varx / vary is definitely not Normally distributed
ggplot(d, aes(var_ratio)) +
  xlim(0,3) +
  geom_histogram(aes(y=..density..), bins=100) +
  geom_density(colour='red') +
  facet_wrap(~n) +
  theme(aspect.ratio = 1)


## NOTE: Turns out, if X ~ N(muX,sigmaX), and Y ~ N(muY,sigmaY), and X and Y are
## independent, then an F(dfx,dfy) distribution with degrees of freedom dfx=nx-1
## and dfy=ny-1 is a nice model for the sampling distribution of (varx / vary).

## My book says this:
## F(dfx, dfy) = (sample_varaince_X / population_variance_X) /
##               (sample_varaince_Y / population_variance_Y)

## I prefer this form:
## F(dfx, dfy) = (sample_variance_X / sample_variance_Y) *
##               (population_variance_Y / population_variance_X)

## My preference for the latter form stems from the fact that under the common
## H0: VarX / Vary = 1, (population_variance_Y / population_variance_X) = 1

x <- seq(0.001, 5, 0.01)
fx1 <- df(x, 1, 1)
fx2 <- df(x, 1, 2)
fx3 <- df(x, 5, 2)
fx4 <- df(x, 10, 1)
fx5 <- df(x, 10, 5)
fx6 <- df(x, 100, 100)

d <- data.table(x, fx1, fx2, fx3, fx4, fx5, fx6)
dd <- melt(d, id.vars='x')

ggplot(dd, aes(x=x, y=value, colour=variable)) +
  geom_line() +
  ylim(0,2.5) +
  theme(aspect.ratio = 1)


## NOTE: To my casual eye, the F and Chisq don't appear all that different. (1)
## If you look closely (as in, think very carefully about the properties of each
## distribution) we will find important differences. (2) They actually aren't
## all that different. In fact, Normal, Chisq, and t distributions are all very
## closely related. You can google it and see for yourself.


## NOTE: back to NHST for variance of two Normal populations
n <- 1000
muX <- 0
varX <- 5
sigmaX <- sqrt(varX)
x <- rnorm(n, muX, sigmaX)

muY <- 0
varY <- 5
sigmaY <- sqrt(varY)
y <- rnorm(n, muY, sigmaY)

## 1. state H's
## H0: sigmaX/sigmaY = 1
## H1: sigmaX/sigmaY < 1
var_ratio_H0 <- 1

## 2. choose confidence
alpha <- 0.05

## 3. choose a statistic to estimate the parameter in H0 and determine its
## sampling distribution.
dfx <- n - 1
dfy <- n - 1
f_obs = var(x) / var(y)

## 4. compute p- and critical- values
p_obs <- pf(F_obs, dfx, dfy, lower.tail = FALSE)
f_crit <- qf(alpha, dfx, dfy, lower.tail = FALSE)

## 5. Make a decision
if(p_obs < alpha) {
  print('reject H0')
} else {
  print('fail to reject H0')
}

if(f_obs > f_crit) {
  print('reject H0')
} else {
  print('fail to reject H0')
}


## Or using R for a one liner
## var.test(x,
##          y,
##          ratio = 1,
##          alternative = c("two.sided", "less", "greater"),...)
var.test(x, y, ratio = 1, alternative = 'greater')


## TODO: If time permits, then do some real data examples of each of the F-tests
## we just looked at. This will be real-time coding... which might be cool for
## the class to see.
