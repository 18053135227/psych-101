library('data.table')
library('ggplot2')
library('gtools')

rm(list = ls())

## NOTE: Types of Error
## | True state of the universe | Our decision about the universe | Type of error |
## |----------------------------+---------------------------------+---------------|
## | H0 True                    | Fail to reject H0               | No error      |
## | H0 True                    | Reject H0                       | Type I error  |
## | H0 False                   | Fail to reject H0               | Type II error |
## | H0 False                   | Reject H0                       | No error      |

## P(type I error) = alpha
## P(type II error) = beta

## P(correctly rejecting H0) = "power"
## P(correctly rejecting H0) = P(reject H0 | H0 is false)
## P(correctly rejecting H0) = 1 - P(Fail to reject H0 | H0 is false)
## P(correctly rejecting H0) = 1 - beta

## To compute any of the H0 False probabilities we need to specify an exact H1.


## NOTE: Types of error in terms of probability distribution
## P(type I error) = alpha = total mass of H0 pmf in the rejection region
## Power = P(correctly rejecting H0) = total mass of H1 pmf in rejection region

## NOTE: Types of error example using Binomial RVs
n <- 30
p_H0 <- 0.4
p_H1 <- 0.7 # must specify exact value to compute power = 1 - beta

## Specify confidence level
alpha <- 0.05

## Compute "critical value":
## In NHST, the critical value is the most extreme value for which we would
## still fail to reject the Null. Equivalently, we reject H0 if we observe a
## value more extreme than the critical value. The critical value is always
## computed from the H0 distribution.
x_crit <- qbinom(alpha, n, p_H0, lower.tail=FALSE)

## Plot H0 and H1 distributions
x <- 0:n
H0 <- dbinom(x, n, p_H0)
H1 <- dbinom(x, n, p_H1)
d <- data.table(x, H0, H1)

## Notice that I am plotting things using a wide format data.table... This is
## new for me, and is honestly pretty exciting.
ggplot(data=d) +
  xlab('Number of successes in n Bernoulli trials') +
  ylab('Probability') +
  geom_point(aes(x=x-0.1, y=H0), colour='red') +
  geom_point(aes(x=x+0.1, y=H1), colour='blue') +
  geom_segment(aes(x=x-0.1, xend=x-0.1, y=0, yend=H0), colour='red') +
  geom_segment(aes(x=x+0.1, xend=x+0.1, y=0, yend=H1), colour='blue') +
  geom_segment(x=x_crit, xend=x_crit, y=0, yend=Inf, colour='black', linetype=3) +
  theme(aspect.ratio = 1) +
  annotate('text', x=x_crit, y=0.2, label='x_crit')


## Shade the regions corresponding to type I error (alpha), type II error
## (beta), and power (1-beta)
ggplot(data=d) +
  xlab('Number of successes in n Bernoulli trials') +
  ylab('Probability') +
  geom_point(aes(x=x-0.1, y=H0)) +
  geom_point(aes(x=x+0.1, y=H1)) +
  geom_line(aes(x=x-0.1, y=H0)) +
  geom_line(aes(x=x+0.1, y=H1)) +
  geom_segment(x=x_crit, xend=x_crit, y=0, yend=Inf, colour='black', linetype=3) +
  geom_segment(x=n*p_H0, xend=n*p_H0, y=0, yend=Inf, colour='black', linetype=3) +
  geom_segment(x=n*p_H1, xend=n*p_H1, y=0, yend=Inf, colour='black', linetype=3) +
  theme(aspect.ratio = 1) +
  geom_ribbon(data=d[x<=x_crit],
              aes(x=x, ymin=0, ymax=H1),
              fill='blue',
              alpha=0.5) +
  geom_ribbon(data=d[x>x_crit],
              aes(x=x, ymin=0, ymax=H1),
              fill='green',
              alpha=0.5) +
  geom_ribbon(data=d[x>x_crit],
              aes(x=x, ymin=0, ymax=H0),
              fill='red',
              alpha=0.5) +
  annotate('text', x=x_crit, y=0.2, label='x_crit', colour='red') +
  annotate('text', x=n*p_H0, y=0.2, label='H0', colour='red') +
  annotate('text', x=n*p_H1, y=0.2, label='H1', colour='red')

## compute type I error (alpha), type II error (beta), and power (1-beta)
alpha <- d[x > x_crit, sum(H0)]
beta <- d[x <= x_crit, sum(H1)]
power <- d[x > x_crit, sum(H1)]
1-beta

## NOTE: The normal distribution
## The normal distribution has density

## f(x) = 1/(sqrt(2 pi) sigma) e^-((x - mu)^2/(2 sigma^2))

## where mu is the mean of the distribution and sigma the standard
## deviation (mu and sigma are parameters that completely define a Normal
## distribution).

## Just like with the binomial distribution (and most other distribution you
## will ever care about) R has it built in.

## ?pnorm
## dnorm(x, mean = 0, sd = 1, log = FALSE)
## pnorm(q, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE)
## qnorm(p, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE)
## rnorm(n, mean = 0, sd = 1)

## If X ~ Normal(mu, sigma), then a sample x, drawn from X can take any value in
## (-Inf to Inf).

## Investigate the probability density function (PDF), cumulative distribution
## function (CDF), and quantile function.
mu <- 0
sigma <- 1

## Note that N(0,1) is called the standard Normal which is the same as the
## Z-distribution

x <- seq(mu-4*sigma, mu+4*sigma, 0.1)
q <- x
p <- seq(0,1,.01)

dN <- dnorm(x, mu, sigma)
pN <- pnorm(q, mu, sigma)
qN <- qnorm(p, mu, sigma)

func_name_d <- rep('probability density', length(dN))
func_name_p <- rep('cumulative probability', length(pN))
func_name_q <- rep('quantile', length(qN))

d <- data.table(
  x = c(x, q, p),
  fx = c(dN, pN, qN),
  type = factor(c(func_name_d, func_name_p, func_name_q),
                levels=c('probability density',
                         'cumulative probability',
                         'quantile'))
)

ggplot(d, aes(x, fx)) +
  geom_line() +
  facet_wrap(~type, scales='free') +
  theme(aspect.ratio = 1)


## NOTE: normal approximation to binomial
## Recall that when n is greater than about 30, X ~ Binom(n,p) is well
## approximated by a Y ~ Normal(mu=n*p, sigma=n*p*(1-p))

# p=.5 appears to be best case scenario... might be fun to fiddle with
n <- 50
p <- 0.5

mu <- n*p
sigma <- sqrt(n*p*(1-p))

xN <- seq(mu-3*sigma, mu+3*sigma, 0.01)
xB <- round(xN)

fxdB <- dbinom(xB, n, p)
fxdN <- dnorm(xN, mu, sigma)

d <- data.table(xB, xN, fxdB, fxdN)

ggplot(data=d) +
  xlab('x') +
  ylab('Probability Density') +
  geom_point(aes(x=xB, y=fxdB), colour='red') +
  geom_line(aes(x=xN, y=fxdN))


## NOTE: Compute and illustrate normal probabilities using the density function
## P(X <= 0)
## TODO: draw stuff on the board
width <- 0.0001 # explore this value from small to large
x <- seq(-5, 0, width)
height <- dnorm(x, mu, sigma)
area <- width * height
sum(width * height)


## NOTE: Compute and illustrate normal probabilities using the cumulative
## probability function
## lower.tail:
## TRUE:  P[X <= x]
## FALSE: P[X > x].

## start by building a useful data.table
mu <- 0
sigma <- 1
x <- seq(mu-4*sigma, mu+4*sigma, 0.01)
fxd <- dnorm(x, mu, sigma)
fxc <- pnorm(x, mu, sigma, lower.tail=TRUE)
d <- data.table(x, fxd, fxc)


## NOTE: P(X <= -1)
xs <- -1
pnorm(xs, mu, sigma, lower.tail=TRUE)

## prepare for plotting by specifying regions under the PDF corresponding to p
## and 1-p
d[, xreg := x <= xs]
d[, xreg := factor(xreg,
                   levels=c('TRUE', 'FALSE'),
                   labels=c('P','1-P'))]

ggplot(d, aes(x, y=fxd, ymin=0, ymax=fxd, fill=xreg)) +
  geom_line() +
  geom_ribbon() +
  geom_vline(xintercept=xs, linetype=3)


## NOTE: P(X > 1)
xs <- 1
pnorm(xs, mu, sigma, lower.tail=FALSE)

## prepare for plotting by specifying regions under the PDF corresponding to p
## and 1-p
d[, xreg := x > xs]
d[, xreg := factor(xreg,
                   levels=c('TRUE', 'FALSE'),
                   labels=c('P','1-P'))]

ggplot(d, aes(x, y=fxd, ymin=0, ymax=fxd, fill=xreg)) +
  geom_line() +
  geom_ribbon() +
  geom_vline(xintercept=xs, linetype=3)


## NOTE: P(0 < X <= 2)
xsl <- 0
xsu <- 2
pnorm(xsl, mu, sigma, lower.tail=TRUE) - pnorm(xsu, mu, sigma, lower.tail=TRUE)

## prepare for plotting by specifying regions under the PDF corresponding to p
## and 1-p
d[, xregl := x > xsl]
d[, xregu := x > xsu]
d[, xreg := x > xsl & x <= xsu]
d[, xreg := factor(xreg,
                   levels=c('TRUE', 'FALSE'),
                   labels=c('P','1-P'))]

ggplot(d, aes(x, y=fxd)) +
  geom_line() +
  geom_ribbon(data=d[x<=xsl], aes(ymin=0, ymax=fxd, fill=xreg, alpha=0.1)) +
  geom_ribbon(data=d[x>xsl & x<=xsu], aes(ymin=0, ymax=fxd, fill=xreg, alpha=0.1)) +
  geom_ribbon(data=d[x>xsu], aes(ymin=0, ymax=fxd, fill=xreg, alpha=0.1)) +
  geom_vline(xintercept=xsl, linetype=3) +
  geom_vline(xintercept=xsu, linetype=3)


## NOTE: Relevance to NHST:
## Recall that in NHST, we reject the Null Hypothesis if P(X < x) < alpha
alpha <- 0.05
x_crit <- qnorm(alpha, mu, sigma, lower.tail=TRUE)

xs <- x_crit
pnorm(xs, mu, sigma, lower.tail=TRUE)

d[, xreg := x < xs]
d[, xreg := factor(xreg,
                   levels=c('TRUE', 'FALSE'),
                   labels=c('P','1-P'))]

ggplot(d, aes(x, y=fxd)) +
  geom_line() +
  geom_ribbon(data=d[x<xs], aes(ymin=0, ymax=fxd, fill=xreg, alpha=0.1)) +
  geom_ribbon(data=d[x>=xs], aes(ymin=0, ymax=fxd, fill=xreg, alpha=0.1)) +
  geom_vline(xintercept=xs, linetype=3) +
  annotate('text', x=x_crit, y=0.3, label='Critical Value') +
  annotate('text', x=mu, y=0.45, label='If this is the Null Distribution:') +
  annotate('text', x=x_crit-1, y=0.1, label='Reject H0') +
  annotate('text', x=mu, y=0.1, label='Fail to Reject H0')


## NOTE: Types of error and power illustration with normal distributions
mu_H0 <- 0.0
mu_H1 <- 3.0
sigma <- 1.0

x <- seq(mu_H0-4*sigma, mu_H1+4*sigma, 0.01)

fxd_H0 <- dnorm(x, mu_H0, sigma)
fxd_H1 <- dnorm(x, mu_H1, sigma)

d <- data.table(x, fxd_H0, fxd_H1)

alpha <- 0.05
x_crit <- qnorm(alpha, mu_H0, sigma, lower.tail=FALSE)

ggplot(data=d, aes(x=x)) +
  ylab('Probability Density') +
  geom_line(aes(y=fxd_H0)) +
  geom_line(aes(y=fxd_H1)) +
  geom_vline(xintercept=x_crit, linetype=3) +
  geom_vline(xintercept=mu_H0, linetype=3) +
  geom_vline(xintercept=mu_H1, linetype=3) +
  ## TODO: add comments / labels for the next three chunks as a class
  geom_ribbon(data=d[x<=x_crit],
              aes(ymin=0, ymax=fxd_H1),
              fill='blue',
              alpha=0.5) +
  geom_ribbon(data=d[x>x_crit],
              aes(ymin=0, ymax=fxd_H1),
              fill='green',
              alpha=0.5) +
  geom_ribbon(data=d[x>x_crit],
              aes(ymin=0, ymax=fxd_H0),
              fill='red',
              alpha=0.5) +
  annotate('text', x=x_crit, y=0.25, label='x_crit') +
  annotate('text', x=mu_H0, y=0.25, label='H0') +
  annotate('text', x=mu_H1, y=0.25, label='H1')


## TODO: Compute types of error with Normal distribution... This is going to be
## a bit awkward for the reason we ran into above.



## NOTE: Sampling distributions
## In NHST, we don't know some population parameter.
## We try to infer it by doing experiments (sampling from a RV)

## In general, a sample is a bunch of numbers.
## We then compute a test statistic from these numbers.
## The test statistic is an estimate for the population parameter of interest.
## The test statistic is also a RV --- TODO: What does this mean???
## The distribution of your test statistic is called the sampling distribution.


## NOTE: What is the sampling distribution of X ~ N(mu, sigma) given we are
## interested in testing a hypothesis about the population parameter mu?

## Our experiment results come from X
n <- 100
mu <- 10.0
sigma <- 2.0
x <- rnorm(n, mu, sigma)

## Testing H about mu -> mu_est = sample mean
## That is, our test statistic is the sample mean
x_obs <- mean(x)

## run the experiment a few times and plot
n_exp <- 6
exp_rec <- c()
rs_rec <- c()
sm_rec <- c()
for(i in 1:n_exp) {
  samp <- rnorm(ns, mu, sigma)
  exp_rec <- c(exp_rec, rep(i, ns))
  rs_rec <- c(rs_rec, samp)
  sm_rec <- c(sm_rec, rep(mean(samp), ns))
}

d <- data.table(exp = exp_rec, rs = rs_rec, sm = sm_rec)

## You can see the sample mean wiggles around the true mean a little differently
## for each experiment
ggplot(d, aes(rs)) +
  geom_histogram(aes(y=..density..), bins=10) +
  geom_vline(aes(xintercept=sm, colour='red')) +
  geom_vline(aes(xintercept=mu, colour='blue')) +
  facet_wrap(~exp)


## NOTE: To examine the distribution of our test statistic, we need to run the
## experiment a bunch of times, record the results each time, and plot a
## histogram of the test statistic.
n_exp <- 100
sm_rec <- c()
for(i in 1:n_exp) {
  x <- rnorm(n, mu, sigma)
  sm <- mean(x)
  sm_rec <- c(sm_rec, sm)
}

d <- data.table(sm=sm_rec)

## TODO: Run a few times and guess the distribution... it may help to play with
## the number of bins.
ggplot(d, aes(sm)) +
  geom_histogram(aes(y=..density..), bins=20) +
  geom_density(colour='red') +
  theme(aspect.ratio=1)


## NOTE: What about if X is not Normally distributed?
## For instance, what if X ~ Uniform(lb,ub)?

## NOTE: Begin by examing the Uniform pdf, cdf, qf

a <- 0.0
b <- 10.0

x <- seq(a,b,.1)
p <- seq(0,1,0.01)
fxd <- dunif(x,a,b)
fxp <- punif(x,a,b)
fxq <- qunif(p,a,b)

d <- data.table(
  id = factor(c(rep('pdf',length(x)),
                rep('cdf', length(x)),
                rep('qf', length(x))),
              levels=c('pdf', 'cdf', 'qf')),
  x = c(x, x, p),
  fx = c(fxd, fxp, fxq)
)

ggplot(d, aes(x, fx)) +
  geom_line() +
  facet_wrap(~id, scales='free') +
  theme(aspect.ratio=1)


## NOTE: mean and variance of X ~ unif(a,b)
## pop_mean = (b-a) / 2
## pop_var = (1/12) * (b-a)^2
## https://en.wikipedia.org/wiki/Uniform_distribution_(continuous)
pop_mean <- (1/2)*(b-a)
pop_var <- (1/12)*(b-a)^2


## NOTE: X ~ Uniform(a,b) --- random sample
a <- 0.0
b <- 10.0

ns <- 100
rs <- runif(ns, a, b)

d <- data.table(rs)

ggplot(d, aes(rs)) +
  geom_histogram(aes(y=..density..), bins=10) +
  geom_density(colour='red')


## NOTE: X ~ Uniform(lb,ub) --- NHST

## Suppose that we draw samples from X ~ Uniform(lb,ub) in some experiment, but
## that we don't know it is a actually a uniform distribution.

## step 1:
## H0: mu = 5
## H1: mu < 5

## step 2:
## We are interested in the parameter mu
## We estimate mu with the sample_mean
## How is sample_mean distributed?


## NOTE: distribution of sample means
n_exp <- 6
exp_rec <- c()
rs_rec <- c()
sm_rec <- c()
for(i in 1:n_exp) {
  samp <- runif(ns, a, b)
  exp_rec <- c(exp_rec, rep(i, ns))
  rs_rec <- c(rs_rec, samp)
  sm_rec <- c(sm_rec, rep(mean(samp), ns))
}

d <- data.table(exp = exp_rec, rs = rs_rec, sm = sm_rec)

## You can see the sample mean wiggles around the true mean a little differently
## for each experiment
ggplot(d, aes(rs)) +
  geom_histogram(aes(y=..density..), bins=10) +
  geom_vline(aes(xintercept=sm, colour='red')) +
  geom_vline(aes(xintercept=(1/2)*(b-a), colour='blue')) +
  facet_wrap(~exp)

## Next, try to estimate the distribution of sample means as a function of the
## number of samples we draw (i.e., the number of experiments).
n_exp <- c(10, 25, 100)
exp_rec <- c()
sm_rec <- c()
for(i in n_exp) {
  for(j in 1:i) {
    samp <- runif(ns, lb, ub)
    exp_rec <- c(exp_rec, i)
    sm_rec <- c(sm_rec, mean(samp))
  }
}

d <- data.table(exp = exp_rec, sm = sm_rec)

## We can see that as n_exp gets large, the distribution of sample means looks
## more and more Normally distributed
ggplot(d, aes(sm)) +
  geom_histogram(aes(y=..density..), bins=20) +
  geom_density(colour='red') +
  facet_wrap(~exp) +
  theme(aspect.ratio=1)


## NOTE: The Central Limit Theorem

## Let X be the RV that we sample from to perform our experiment
## Let Y = (1/N) * sum(X)

## The "Central Limit Theorem" tells us:

## Y ~ N(muY, sigmaY)

## muY = E(Y) = E(X)
## sigmaY^2 = Var(Y) = Var(X) / n
## sigmaY = SD(Y) = SD(X) / sqrt(n)

## Proof: Beyond the scope of this class

## Implications: HUGE!!! The central limit theorem means that we have a very
## good idea about how sample mean test statistics are distributed, regardless
## of what RV is generating the results of our experiment. If you are using
## sample means to estimate a population mean, then is will likely be
## distributed Normally (if n is large enough). As we will see below, this is
## why t.tests are so prevalent.


## NOTE: verify the mean and variance of the distribution of sample means?

## We can see that the sample mean of the distribution of sample means seems to
## converge to the population mean of our original X ~ unif(a,b).
d[, smsm := mean(sm), .(exp)]
ggplot(d, aes(sm)) +
  geom_histogram(aes(y=..density..), bins=20) +
  geom_density(colour='red') +
  geom_vline(aes(xintercept=smsm), colour='red') +
  geom_vline(aes(xintercept=pop_mean), colour='blue') +
  facet_wrap(~exp) +
  theme(aspect.ratio=1)


## TODO: fold these into the demo for the central limit theorem
## TODO: Or else make them homework problems
## TODO: The goal is to get a feel for how quickly and reliably different RVs converge to normal via the central limit theorem.
## NOTE: X ~ t(df)
## NOTE: X ~ Normal(mu, sigma)
## NOTE: X ~ F(d1, d2)


## NOTE: Armed with the central limit theorem, we are ready to return to NHST.

## Suppose you run an experiment and the results are:
n <- 100
a <- 0.0
b <- 50.0
x <- runif(n, a, b)

## Further suppose that we don't know the population mean of X, but we do know
## the population variance (this is not realistic but just a good starting
## point).
pop_var <- (1/12)*(b-a)^2

## step 1:
## H0: mu = 10.0
## H1: mu > 10.0
mu_H0 <- 10.0

## step 2:
## mu_est = sample_mean ~ Normal(mu, sigma/n)
x_obs <- mean(x)

## step 3: compute P(x_obs|p_H0)
pH0 <- pnorm(x_obs, mu_H0, sqrt(pop_var), lower.tail=FALSE)

## step 4:
alpha <- 0.05

## step 5:
## Reject H0 if P(X < x_obs | H0 is true) < alpha
pH0 < alpha


## NOTE: Repeat previous example assuming unknown population variance... which
## is pretty much the situation real life always puts you in.

## Everything is the same except now we have to estimate the population
## variance. It's tempting to just do this:

## H0 <- pnorm(x_obs, mu_H0, sd(x)/sqrt(n), lower.tail=FALSE)

## However, this wouldn't be quite right. When we don't know the variance of the
## sampling distribution, our test statistic must be modeled as a t-distribution
## instead of a Normal distribution. In general, a t distribution is very
## similar to the "standard Normal" distribution Z ~ N(mu=0,sigma=1), except the
## t has heavier tails. This means that to compute pH0, we need to convert x_obs
## to t_obs.


## NOTE: begin by investigating the t-distribution
## Student's t-distribution has one parameter called the "degrees of freedom"
## df = n-1
x <-  seq(-3,3,.01)
pdf_t10 <- dt(x, 5)
pdf_t50 <- dt(x, 10)
pdf_t100 <- dt(x, 20)
pdf_N <- dnorm(x, 0, 1)

d <- data.table(x, pdf_t10, pdf_t50, pdf_t100, pdf_N)
dd <- melt(d, id.vars="x")

ggplot(dd, aes(x=x, y=value, colour=variable)) +
  geom_line() +
  theme(aspect.ratio=1)

alpha <- 0.05
z_crit <- qnorm(alpha, 0, 1)
t_crit <- qt(alpha, 10-1)

## TODO: Say something smart about this figure
ggplot(data=d, aes(x=x)) +

  geom_line(aes(y=pdf_N), colour='red') +
  geom_vline(xintercept=z_crit, colour='red', linetype=3) +
  geom_ribbon(data=d[x<=z_crit],
              aes(ymin=0, ymax=pdf_N),
              fill='red',
              alpha=0.25) +

  geom_line(aes(y=pdf_t10), colour='blue') +
  geom_vline(xintercept=t_crit, colour='blue', linetype=3) +
  geom_ribbon(data=d[x<=t_crit],
              aes(ymin=0, ymax=pdf_t10),
              fill='blue',
              alpha=0.25) +

  theme(aspect.ratio=1)


## NOTE: converting x_obs to t_obs
## the sampling distribution for x_obs comes from X ~ N(mu, sigma/sqrt(n))
## the sampling distribution for t_obs comes from t, E(t)=0, Var(t)=1

## Luckily, it's easy to "standardize" observations from random variables.

## Suppose you run an experiment and the results are:
n <- 100
a <- 0.0
b <- 50.0
x <- runif(n, a, b)

## step 1:
## H0: mu = 15.0
## H1: mu > 15.0
mu_H0 <- 15.0

## step 2:
## mu_est = sample_mean ~ Normal(mu, sigma/n)
x_obs <- mean(x)
t_obs <- (x_obs - mu_H0) / (sd(x)/sqrt(n)) # standardize x_obs

pH0 <- pt(t_obs, n-1, lower.tail=FALSE)

alpha <- 0.05
pH0 < alpha

## NOTE: do all the previous example using t.test()
t.test(x, mu=mu_H0, alternative='greater')


## TODO: point estimation and confidence intervals
## TODO: (week 6) comparing two treatments (paired and independent samples t-tests)
## TODO: (week 7) multiple treatments omnibus test (ANOVA)
## TODO: (week 8) co-variation, correlation, regression
