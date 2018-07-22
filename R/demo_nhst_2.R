library('data.table')
library('ggplot2')
library('gtools')

rm(list = ls())

## NOTE: Review - Types of Error
## | True state of the universe | Our decision about the universe | Type of Error |
## |----------------------------+---------------------------------+---------------|
## | H0 True                    | Fail to reject H0               | No error      |
## | H0 True                    | Reject H0                       | Type I error  |
## | H0 False                   | Fail to reject H0               | Type II error |
## | H0 False                   | Reject H0                       | No error      |


## NOTE: Might be useful to read this table like this:
## (1) IF (True state of the universe)
## (2) AND (Our decision about the universe)
## (3) THEN we have committed a (Type of Error)


# NOTE: Review - Types of error and power with X ~ N(mu, sigma)
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


## NOTE: Power as a function of difference between mu_H0 and mu_H1
mu_H0 <- 0.0
sigma <- 1.0

alpha <- 0.05
x_crit <- qnorm(alpha, mu_H0, sigma, lower.tail=FALSE)

mu_H1 <- c(1.0, 2.0, 3.0)

d_rec <- list()
for(i in 1:length(mu_H1)) {
  ## define a sample space that ranges from 4 SDs left of mu_H0 to 4 SDs right
  ## of mu_H1
  x <- seq(mu_H0-4*sigma, mu_H1[i]+4*sigma, 0.01)

  ## compute the pdf for H0 and H1
  fxd_H0 <- dnorm(x, mu_H0, sigma)
  fxd_H1 <- dnorm(x, mu_H1[i], sigma)

  ## create data.table
  d <- data.table(i, mu_H0, mu_H1=mu_H1[i], x, fxd_H0, fxd_H1)

  ## add d to our record
  ## TODO: Why [[]]?
  d_rec[[i]] <- d
}

## Make one data.table from a list of many
dd <- rbindlist(d_rec)

## plot results
ggplot(dd, aes(x=x)) +
  facet_wrap(~i) +
  theme(aspect.ratio=1.0) +
  ylab('Probability Density') +
  geom_line(aes(y=fxd_H0)) +
  geom_line(aes(y=fxd_H1)) +
  geom_vline(xintercept=x_crit, linetype=1, colour='red') +
  geom_vline(xintercept=mu_H0, linetype=3) +
  geom_vline(aes(xintercept=mu_H1), linetype=3) +
  geom_ribbon(data=dd[x<=x_crit],
              aes(ymin=0, ymax=fxd_H1),
              fill='blue',
              alpha=0.5) +
  geom_ribbon(data=dd[x>x_crit],
              aes(ymin=0, ymax=fxd_H1),
              fill='green',
              alpha=0.5) +
  geom_ribbon(data=dd[x>x_crit],
              aes(ymin=0, ymax=fxd_H0),
              fill='red',
              alpha=0.5)


## NOTE: Power as a function of sigma
mu_H0 <- 0.0
mu_H1 <- 2.0
sigma <- c(0.75, 1.5, 3.0)
alpha <- 0.05

d_rec <- list()
for(i in 1:length(sigma)) {
  ## since sigma is changing in each iteration, we need to move it inside the
  ## for loop
  x_crit <- qnorm(alpha, mu_H0, sigma[i], lower.tail=FALSE)

  ## define a sample space that ranges from 4 SDs left of mu_H0 to 4 SDs right
  ## of mu_H1
  x <- seq(mu_H0-4*sigma[i], mu_H1+4*sigma[i], 0.01)

  ## compute the pdf for H0 and H1
  fxd_H0 <- dnorm(x, mu_H0, sigma[i])
  fxd_H1 <- dnorm(x, mu_H1, sigma[i])

  ## create data.table
  d <- data.table(i, x_crit, x, fxd_H0, fxd_H1)

  ## add d to our record
  ## TODO: Why [[]]?
  d_rec[[i]] <- d
}

## Make one data.table from a list of many
dd <- rbindlist(d_rec)

## plot results
ggplot(dd, aes(x=x)) +
  facet_wrap(~i) +
  theme(aspect.ratio=1.0) +
  ylab('Probability Density') +
  geom_line(aes(y=fxd_H0)) +
  geom_line(aes(y=fxd_H1)) +
  geom_vline(aes(xintercept=x_crit), linetype=1, colour='red') +
  geom_vline(xintercept=mu_H0, linetype=3) +
  geom_vline(xintercept=mu_H1, linetype=3) +
  geom_ribbon(data=dd[x<=x_crit],
              aes(ymin=0, ymax=fxd_H1),
              fill='blue',
              alpha=0.5) +
  geom_ribbon(data=dd[x>x_crit],
              aes(ymin=0, ymax=fxd_H1),
              fill='green',
              alpha=0.5) +
  geom_ribbon(data=dd[x>x_crit],
              aes(ymin=0, ymax=fxd_H0),
              fill='red',
              alpha=0.5)


## NOTE: Review - Sampling Distribution

## Perform an experiment
## Same thing as sample from a random variable X ~ Whatever(p1, p2, ..., p_whatever)
## Pick a sample size n (i.e, take n measurements)
## Results: {x1, x2, x3,..., xn}

## In psychology, each x could be a single measurement from different subjects,
## or different measurements from the same person, or some combination.
## Typically, there is some room for creativity in how "measurements" are
## defined. You will get to wrestle with this in your final project.

## Suppose we are interested mu_X = E[X]
## More specifically, suppose we are interested in
## H0: mu_X = 50
## H1: mu_X > 50

## Then we use our sample (observed data) to estimate mu_X
## mu_X_hat = mu_X_est = x_bar = x_obs = mean({x1, x2, x3, ..., xn})

## In general, we reject H0 if P(X_obs > x_obs | H0) < alpha

## Here is the key to the importance of the sampling distribution:
## x_obs isn't a sample from X ~ Whatever(p1, p2, ..., pn)
## x_obs is a sample from X_bar ~ mean(X) = mean(Whatever(p1, p2, ..., pn))

## So really our decision rule is:
## Reject if P(x_obs | H0) < alpha
## Reject if P(X_bar > x_obs | H0) < alpha

## We've just been lazy with how we've been writing it...

## Q: What do we need to know in order to compute P(X_obs > x_obs | H0)?
## A: We need to know the distribution of X_bar
## A: We need to know the distribution of sample means

## The central limit theorem tells us that (if n is large enough):
## X_bar ~ N(mu_X, sigma_X/sqrt(n))

## Q: How large does n have to be?
## A: Good question...


## NOTE: add sample size to power computation
## Start with the assumption that the diiference between H1 and H2 is:
mu_x_H0 <- 0.0
mu_x_H1 <- 5.0

## Further suppose that you know sigma_x
sigma_x <- 3.0

# specifiy sample size
n <- 100

## specifiy sampling distribution
mu_x_obs_H0 <- mu_x_H0
mu_x_obs_H1 <- mu_x_H1
sigma_x_obs <- sigma_x / sqrt(n)

## compute critical value
alpha <- 0.05
x_crit <- qnorm(alpha, mu_x_H0, sigma_x, lower.tail=FALSE)

## compute power
pnorm(x_crit, mu_x_obs_H1, sigma_x_obs, lower.tail=FALSE)

## TODO: Rats... didn't get to this.
## Notice that n descreases the variance of the sampling distribution, and this
## increases power.
## Q: What n do you need to for sure get power >= 0.8?
## A: Construct a function that maps n to power


## NOTE: Armed with the central limit theorem, we are ready to return to NHST.

rm(list=ls())

## Suppose you run an experiment and the results are:
n <- 100
a <- 0.0
b <- 50.0
x <- runif(n, a, b)

## Further suppose that we don't know the population mean of X, but we do know
## the population variance (this is not realistic but just a good starting
## point).
pop_var <- (1/12)*(b-a)^2
pop_sd <- sqrt(pop_var)


## NOTE: Test H1 greater than
## step 1:
## H0: mu = 25.0
## H1: mu > 25.0
mu_x_H0 <- 25.0

## step 2:
alpha <- 0.05

## step 3:
x_obs <- mean(x)

## step 4: compute P(X_obs > x_obs | p_H0)
mu_x_obs_H0 <- mu_x_H0

n <- length(x)
sigma_x_obs <- pop_sd / sqrt(n)

pH0 <- pnorm(x_obs, mu_x_H0, sigma_x_obs, lower.tail=FALSE)
x_crit <- qnorm(alpha, mu_x_H0, sigma_x_obs, lower.tail=FALSE)

## step 5:
## Reject H0 if P(X_obs < x_obs | H0 is true) < alpha
pH0 < alpha
x_obs > x_crit

## plot the results
mu <- mu_x_obs_H0
sigma <- sigma_x_obs
x <- seq(mu-4*sigma, mu+4*sigma, 0.1)
fxd <- dnorm(x, mu, sigma)
d <- data.table(x, fxd)

ggplot(d, aes(x, fxd)) +
  geom_line() +
  geom_vline(xintercept = x_crit, linetype=3, colour='red') +
  geom_vline(xintercept = x_obs, linetype=3, colour='blue') +
  annotate('text', x=x_crit, y=0.25, label='x_crit') +
  annotate('text', x=x_obs, y=0.25, label='x_obs') +
  theme(aspect.ratio=1)


## NOTE: Test H1 less than
## step 1:
## H0: mu = 25.0
## H1: mu < 25.0
mu_x_H0 <- 25.0

## step 2:
alpha <- 0.05

## step 3:
x_obs <- mean(x)

## step 4: compute P(x_obs|p_H0)
mu_x_obs_H0 <- mu_x_H0

n <- length(x)
sigma_x_obs <- pop_sd / sqrt(n)

pH0 <- pnorm(x_obs, mu_x_H0, sigma_x_obs, lower.tail=TRUE)
x_crit <- qnorm(alpha, mu_x_H0, sigma_x_obs, lower.tail=TRUE)

## step 5:
## Reject H0 if P(X < x_obs | H0 is true) < alpha
pH0 < alpha
x_obs < x_crit

## plot the results
mu <- mu_x_obs_H0
sigma <- sigma_x_obs
x <- seq(mu-4*sigma, mu+4*sigma, 0.1)
fxd <- dnorm(x, mu, sigma)
d <- data.table(x, fxd)

ggplot(d, aes(x, fxd)) +
  geom_line() +
  geom_vline(xintercept = x_crit, linetype=3, colour='red') +
  geom_vline(xintercept = x_obs, linetype=3, colour='blue') +
  annotate('text', x=x_crit, y=0.25, label='x_crit') +
  annotate('text', x=x_obs, y=0.25, label='x_obs') +
  theme(aspect.ratio=1)


## NOTE: Test H1 different from H0 (in any direction)
## step 1:
## H0: mu = 25.0
## H1: mu != 25.0
mu_x_H0 <- 25.0

## step 2:
alpha <- 0.05

## step 3:
x_obs <- mean(x)

## step 4: compute P(x_obs|p_H0)
mu_x_obs_H0 <- mu_x_H0

n <- length(x)
sigma_x_obs <- pop_sd / sqrt(n)

pH0_less <- pnorm(x_obs, mu_x_H0, sigma_x_obs, lower.tail=TRUE)
pH0_greater <- pnorm(x_obs, mu_x_H0, sigma_x_obs, lower.tail=FALSE)

x_crit_less <- qnorm(alpha/2, mu_x_H0, sigma_x_obs, lower.tail=TRUE)
x_crit_greater <- qnorm(alpha/2, mu_x_H0, sigma_x_obs, lower.tail=FALSE)

## step 5:
## Reject H0 if P(X < x_obs | H0 is true) < alpha
pH0 < alpha
x_obs < x_crit

## plot the results
mu <- mu_x_obs_H0
sigma <- sigma_x_obs
x <- seq(mu-4*sigma, mu+4*sigma, 0.1)
fxd <- dnorm(x, mu, sigma)
d <- data.table(x, fxd)

ggplot(d, aes(x, fxd)) +
  geom_line() +
  geom_vline(xintercept = x_crit_less, linetype=3, colour='red') +
  geom_vline(xintercept = x_crit_greater, linetype=3, colour='red') +
  geom_vline(xintercept = x_obs, linetype=3, colour='blue') +
  annotate('text', x=x_crit_less, y=0.25, label='x_crit_less') +
  annotate('text', x=x_crit_greater, y=0.25, label='x_crit_greater') +
  annotate('text', x=x_obs, y=0.25, label='x_obs') +
  theme(aspect.ratio=1)


## Compare one-sided vs two-sided critical values
## TODO: Say something smart about this plot. Thoughts?
ggplot(d, aes(x, fxd)) +
  geom_line() +
  geom_vline(xintercept = x_crit, linetype=3, colour='black') +
  geom_vline(xintercept = x_crit_less, linetype=3, colour='red') +
  geom_vline(xintercept = x_crit_greater, linetype=3, colour='red') +
  geom_vline(xintercept = x_obs, linetype=3, colour='blue') +
  annotate('text', x=x_crit_less, y=0.25, label='x_crit_less') +
  annotate('text', x=x_crit_greater, y=0.25, label='x_crit_greater') +
  annotate('text', x=x_crit, y=0.2, label='x_crit') +
  annotate('text', x=x_obs, y=0.25, label='x_obs') +
  theme(aspect.ratio=1)


## NOTE: Repeat previous examples assuming unknown population variance... which
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


## NOTE: Begin by investigating the t-distribution
## Student's t-distribution has one parameter called the "degrees of freedom"
## df = n-1
x <-  seq(-3,3,.01)
pdf_t5 <- dt(x, 5)
pdf_t10 <- dt(x, 10)
pdf_t20 <- dt(x, 20)
pdf_N <- dnorm(x, 0, 1)

d <- data.table(x, pdf_t5, pdf_t10, pdf_t20, pdf_N)
dd <- melt(d, id.vars="x")

## t-distribution looks very normal, but with heavy tails
ggplot(dd, aes(x=x, y=value, colour=variable)) +
  geom_line() +
  theme(aspect.ratio=1)

alpha <- 0.05
z_crit <- qnorm(alpha, 0, 1)
t_crit <- qt(alpha, 10-1)

## Heavier tails means you'll need more extreme data to reject H0
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
rm(list=ls())

n <- 100
a <- 0.0
b <- 50.0
x <- runif(n, a, b)

## step 1:
## H0: mu = 15.0
## H1: mu > 15.0
mu_H0 <- 15.0

alpha <- 0.05

## step 2:
## mu_est = sample_mean ~ Normal(mu, sigma/n)
x_obs <- mean(x)

## We don't know sigma, so we must estimate it
sigma_X_est <- sd(x)
sigma_Xbar_est <- sigma_X_est / sqrt(n)

## standardize x_obs
t_obs <- (x_obs - mu_H0) / sigma_Xbar_est

df <- n-1
pH0 <- pt(t_obs, df, lower.tail=FALSE)
t_crit <- qt(alpha, df, lower.tail=FALSE)

pH0 < alpha


## NOTE: do all of the previous example using t.test()
t.test(x, mu=mu_H0, alternative='greater')


## NOTE: plot these results
t <- seq(-4, 4, 0.01)
ft <- dt(t, df)
d <- data.table(t,ft)

ggplot(d, aes(x=t, y=ft)) +
  geom_line() +
  geom_vline(xintercept=t_crit, colour='red', linetype=3) +
  geom_vline(xintercept=t_obs, colour='blue', linetype=3)


## NOTE: t-tests on real world data (flights?)
library(hflights)
str(hflights)
d <- as.data.table(hflights)

## Is the population mean DepDelay greater than 10?
## 1. state the Hs
## H0: mu = 5
## H1: mu > 5
mu_H0 <- 5

## 2. state confidence
alpha <- 0.05

## 3. compute x_obs and t_obs
x <- d[, mean(DepDelay, na.rm=TRUE), .(UniqueCarrier, Month)][, V1]
n <- length(x)

x_obs <- mean(x)
sigma_xbar_est <- sd(x) / sqrt(n)

t_obs <- (x_obs - mu_H0) / sigma_xbar_est

## 4. compute P(t > t_obs | H0) and t_crit
df <- n-1
pH0 <- pt(t_obs, df, lower.tail=FALSE)
t_crit <- qt(alpha, df, lower.tail=FALSE)

## 5. Make a decision
pH0 < alpha
t_obs > t_crit


## NOTE: do it all via t.test
t.test(x, mu=mu_H0, alternative='greater')


## NOTE plot it
t <- seq(-4, 4, 0.01)
ft <- dt(t, df)
d <- data.table(t,ft)

ggplot(d, aes(x=t, y=ft)) +
  geom_line() +
  geom_vline(xintercept=t_crit, colour='red', linetype=3) +
  geom_vline(xintercept=t_obs, colour='blue', linetype=3)


## NOTE: Another Real-world example
rm(list = ls())

dir_data_delay = '../final_project/data/delay/'
files_delay <- list.files(dir_data_delay)

d_rec <- list()
for(i in 1:length(files_delay)) {
  f_name <- files_delay[i]
  f <- paste(dir_data_delay, f_name, sep='')
  d <- fread(f)
  d[, fname := f_name]
  d[, condition := 'delay']
  d_rec[[i]] <- d
}

d <- rbindlist(d_rec)

col_names <- c(
  't',
  't_prob',
  'bnd',
  'cat',
  'x',
  'y',
  'rsp',
  'rt'
)

setnames(d, 1:8, col_names)

d[, acc := cat == rsp]

## Treating all people as being identifical, did they guess during the first 20
## trials?
dd <- d[, .SD[1:20], .(fname)]

## 1.
## H0: p = .5
## H1: p > .5
p_H0 <- .5

## 2.
alpha <- .05

## 3.
## X ~ Bernoulli(p)
## X_obs ~ binom(n, p)

## Estimate the population parameter p
n <- dd[, .N]
x <- dd[, acc]
x_obs <- sum(x, na.rm=TRUE)

## 4. P(X > x_obs)
p_obs <- pbinom(x_obs-1, n, p_H0, lower.tail=FALSE)
x_crit <- qbinom(alpha, n, p_H0, lower.tail=FALSE)

## 5.
p_obs < alpha
x_obs > x_crit


## NOTE: using binom.test
binom.test(x_obs, n, p_H0, alternative='greater')


## NOTE: try it with X = Number of problems solved (nps)
rm(list = ls())

dir_data_delay = '../final_project/data/delay/'
files_delay <- list.files(dir_data_delay)

d_rec <- list()
for(i in 1:length(files_delay)) {
  f_name <- files_delay[i]
  f <- paste(dir_data_delay, f_name, sep='')
  d <- fread(f)
  d[, fname := f_name]
  d[, condition := 'delay']
  d_rec[[i]] <- d
}

d <- rbindlist(d_rec)

col_names <- c(
  't',
  't_prob',
  'bnd',
  'cat',
  'x',
  'y',
  'rsp',
  'rt'
)

setnames(d, 1:8, col_names)

d[, acc := cat == rsp]

## Begin NPS stuff
nps <- d[, sum(t_prob == 1)-1, .(fname)]

## 1.
## H0: mu = 8
## H1: mu < 8
mu_x_H0 <- 8

## 2.
alpha <- 0.05

## 3.
x <- nps[, V1]
x_obs <- mean(x)

## 4.
mu_x_obs_H0 <- mu_x_H0

n <- nps[, .N]
sig_x_obs <- sd(x) / sqrt(n)

t_obs <- (x_obs - mu_x_H0) / sig_x_obs

df <- n-1
p_H0 <- pt(t_obs, df, lower.tail=TRUE)
t_crit <- qt(alpha, df, lower.tail=TRUE)

## 5.
p_H0 < alpha
t_obs < t_crit

## use t.test
t.test(x, mu=mu_x_H0, alternative='less')

## plot it
t <- seq(-4, 4, 0.01)
ft <- dt(t, df)
d <- data.table(t,ft)

ggplot(d, aes(x=t, y=ft)) +
  geom_line() +
  geom_vline(xintercept=t_crit, colour='red', linetype=3) +
  geom_vline(xintercept=t_obs, colour='blue', linetype=3)


## NOTE: try it with X = trials to criterion (t2c)
rm(list = ls())

dir_data_delay = '../final_project/data/delay/'
files_delay <- list.files(dir_data_delay)

d_rec <- list()
for(i in 1:length(files_delay)) {
  f_name <- files_delay[i]
  f <- paste(dir_data_delay, f_name, sep='')
  d <- fread(f)
  d[, fname := f_name]
  d[, condition := 'delay']
  d_rec[[i]] <- d
}

d <- rbindlist(d_rec)

col_names <- c(
  't',
  't_prob',
  'bnd',
  'cat',
  'x',
  'y',
  'rsp',
  'rt'
)

setnames(d, 1:8, col_names)

d[, acc := cat == rsp]

## Begin t2c stuff
t2c <- d[, mean(diff(which(t_prob == 1))), .(fname)]

## flag suspicious subs for later inspection
t2c[!is.finite(V1), fname]

## For now, simply remove them from consideraation
t2c <- t2c[is.finite(V1)]

## 1.
## H0: mu = 8
## H1: mu > 8
mu_x_H0 <- 60

## 2.
alpha <- 0.05

## 3.
x <- t2c[, V1]
x_obs <- mean(x)

## 4.
mu_x_obs_H0 <- mu_x_H0

n <- t2c[, .N]
sig_x_obs <- sd(x) / sqrt(n)

t_obs <- (x_obs - mu_x_H0) / sig_x_obs

df <- n-1
p_H0 <- pt(t_obs, df, lower.tail=FALSE)
t_crit <- qt(alpha, df, lower.tail=FALSE)

## 5.
p_H0 < alpha
t_obs < t_crit

## use t.test
t.test(x, mu=mu_x_H0, alternative='greater')

## plot it
t <- seq(-4, 4, 0.01)
ft <- dt(t, df)
d <- data.table(t,ft)

ggplot(d, aes(x=t, y=ft)) +
  geom_line() +
  geom_vline(xintercept=t_crit, colour='red', linetype=3) +
  geom_vline(xintercept=t_obs, colour='blue', linetype=3)


## TODO: point estimation and confidence intervals
## TODO: (week 6) comparing two treatments (paired and independent samples t-tests)
## TODO: (week 7) multiple treatments omnibus test (ANOVA)
## TODO: (week 8) co-variation, correlation, regression

