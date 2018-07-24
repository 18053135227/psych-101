library('data.table')
library('ggplot2')
library('gtools')

rm(list = ls())

## read some cool data
f <- '../data/switch_cat_learn/switch_data.csv'
d <- fread(f)

## there's a bit going on here... lets break it down
d[, unique(subject)]
d[, unique(condition)]
d[, unique(phase)]
d[, unique(cue)]

## Plot it
dd <- d[, .(mean(acc), sd(acc)/sqrt(.N)), .(condition, phase, block, cue)]
setnames(dd, c('V1','V2'), c('acc_mean', 'acc_err'))
ggplot() +
  geom_line(data=dd[phase==1], aes(x=block, y=acc_mean, colour=factor(cue))) +
  geom_errorbar(data=dd[phase==1],
                aes(x=block,
                    ymin=acc_mean-acc_err,
                    ymax=acc_mean+acc_err,
                    colour=factor(cue))) +
  geom_line(data=dd[phase==2], aes(x=block, y=acc_mean, colour=factor(cue))) +
  geom_errorbar(data=dd[phase==2],
                aes(x=block,
                    ymin=acc_mean-acc_err,
                    ymax=acc_mean+acc_err,
                    colour=factor(cue))) +
  geom_line(data=dd[phase==3], aes(x=block, y=acc_mean, colour=factor(cue))) +
  geom_errorbar(data=dd[phase==3],
                aes(x=block,
                    ymin=acc_mean-acc_err,
                    ymax=acc_mean+acc_err,
                    colour=factor(cue))) +
  geom_line(data=dd[phase==4], aes(x=block, y=acc_mean, colour=factor(cue))) +
  geom_errorbar(data=dd[phase==4],
                aes(x=block,
                    ymin=acc_mean-acc_err,
                    ymax=acc_mean+acc_err,
                    colour=factor(cue))) +
  facet_wrap(~condition)


## NOTE: Q: Is the mean accuracy greater than .5 in condition 1 during phase 1?
## Assume that all subjects are identiccal.

## H0: mu = .5
## H1: mu > .5

## Option 1:
## X = correct or incorrect trial
## X ~ Bernoulli(p)
## X_obs ~ Binomial(n, p)
x <- d[condition==1 & phase==1, acc, .(trial)][, acc]
n <- length(x)

## Does the length of x make sense?
## Number of subjects in condition 1 and phase 1
ns <- d[condition==1 & phase==1, length(unique(subject))]

## Number of blocks in condition 1 and phase 1
nt <- d[condition==1 & phase==1, length(unique(trial))]

## Yep. This is how many observations we expected.
ns*nt == n

## Back to the test
pH0 <- .5
x_obs <- sum(x)
binom.test(x_obs, n, pH0, alternative='greater', conf.level=.95)


## Option 2:
## X = mean accuracy on a block of trials
## X ~ Normal(mu, sigma)
## X_obs ~ Normal(mu, sigma/sqrt(n))
## In this case, each block of phase 1 from each subject is one sample from X
x <- d[condition==1 & phase==1, mean(acc), .(block, subject)][, V1]
n <- length(x)

## Does the length of x make sense?
## Number of subjects in condition 1 and phase 1
ns <- d[condition==1 & phase==1, length(unique(subject))]

## Number of blocks in condition 1 and phase 1
nb <- d[condition==1 & phase==1, length(unique(block))]

## Yep. This is how many observations we expected.
ns*nb == n

## Back to the test
muH0 <- .5
t.test(x, mu=muH0, alternative='greater', conf.level=.95)


## NOTE: Comparing two treatments --- Independent samples

## Is there a difference in accuracy between condition 1 and condition 2 during
## phase 1?

## X = RV that generates data for condition 1
## Y = RV that generates data for condition 2

## Condition 1 and Condition 2 contain different subjects.
## That is, Condition is a "between-subjects" factor.
## This allows us to safely assume X and Y are independent.

## Step 1:
## H0: muX = muY
## H1: muX != muY

## H0: muX - muY = 0
## H1: muX - muY != 0

## Step 2:
## Choose a confidence level

## Step 3: Pick a statistic to estimate the parameter in step 1

## What might provide a good estimate for muX - muY?

## Q: In general, what is a good estimator of a parameter p?
## A: A good estimate has two properties:
## (1) It is unbiased: E[p_hat] = p
## (2) It has minimum variance Var[p_hat] is the smallest possible
## Estimates that have these properties are called MVUES:
## Minimum Variance Unbiased Estimators


## NOTE: Example of a biased estimator: sample standard deviation as an
## estimator for population standard deviation.

## For an unbiased estimator of a parameter, p, E[p_hat] = p.

## Bias = E[p_hat] - p

## The parameter we are trying to estimate is p = population standard deviation.
## Below, we investigate the estimators sd_biased and sd_unbiased.

## (1/n) normalization term
sd_biased <- function(x) {
  n <- length(x)
  v <- (1/n)*sum((x - mean(x))^2)
  s <- sqrt(v)
  return(s)
}

## (1/(n-1)) normalization term
sd_unbiased <- function(x) {
  n <- length(x)
  v <- (1/(n-1))*sum((x - mean(x))^2)
  s <- sqrt(v)
  return(s)
}

## Pick population parameters for X ~ N(mu, sigma)
mu <- 0
sigma <- 10

## Pick the sample sizes you would like to explore
n_samples <- c(2, 5, 10, 50)

## Pick the number of times you'd like to repeat each experiment
n_exps <- 1000

## Run the experiments
sd_biased_rec <- c()
sd_unbiased_rec <- c()
n_rec <- c()
for(n in n_samples) {
  for(i in 1:n_exps) {
    ## draw a random sample of size n from N(mu, sigma)
    x <- rnorm(n, mu, sigma)

    ## append new result to end of rec vectors
    sd_biased_rec <- c(sd_biased_rec, sd_biased(x))
    sd_unbiased_rec <- c(sd_unbiased_rec, sd_unbiased(x))
    n_rec <- c(n_rec, n)
  }
}

## Turn rec vectors into a data.table
d <- data.table(sd_biased = sd_biased_rec,
                sd_unbiased = sd_unbiased_rec,
                n = n_rec)

## Compute the bias of our estimators: B = E[p_hat] - p
## Above, we set p = population standard deviation = sigma
## Here, we aren't set up to compute E[p_hat] exactly, but we can estimate it
## with mean(p_hat).
d[, mean(sd_biased) - sigma, .(n)]
d[, mean(sd_unbiased) - sigma, .(n)]

## Compute Bias = E[sd_basied] - sigma
d[, mean(sd_biased) - sigma, .(n)]
d[, mean(sd_unbiased) - sigma, .(n)]

## We can see that both appear slightly biased, the bias of both decreases with
## increasing sample size, and that sd_unbiased is less biased. In R, when you
## cal sd() on a vector of numbers, you are actually invoking our sd_unbiased.

## Examine the bias of these estimators graphically
dd <- melt(d, measure.vars = c('sd_biased','sd_unbiased'))
dd[, sd_est := mean(value), .(n)]
ggplot(dd, aes(value, fill=variable)) +
  geom_histogram(position='identity', bins=20, alpha=0.5) +
  ## TODO: why isn't this working... am I just loooking at it wrong?
  geom_vline(aes(xintercept=sd_est, colour=variable)) +
  geom_vline(xintercept = sigma, linetype=3) +
  facet_wrap(~n, scale = 'free')


## NOTE: Back to the test
## Is there a difference in accuracy between condition 1 and condition 2 during
## phase 1?

## X = RV that generates data for condition 1
## Y = RV that generates data for condition 2
## X and Y are independent

## Step 1:
## H0: muX - muY = 0
## H1: muX - muY != 0

## Step 2:
## Choose a confidence level


## Step 3: Pick a statistic to estimate the parameter in step 1
## NOTE: We again need to go on a quick side rant to get through step 3

## In this particular scenario, we want an estimate for mu_diff = muX - muY:
## Call our estimator mu_diff_hat.
## Then we want,
## E[mu_diff_hat] = muX - muY

## Intuition suggests that the following might be good:

## mu_diff_hat = Z_bar
## Z_bar = X_bar - Y_bar

## Here, X_bar and Y_bar are the RVs that generate the distribution of sample
## means for X and Y.

## Recall that,

## X_bar = mean(X)
## E[X_bar] = E[X] = muX
## Var[X_bar] = Var[X] / n = sigmaX^2 / n

## Y_bar = mean(Y)
## E[Y_bar] = E[Y] = muY
## Var[Y_bar] = Var[Y] / n = sigmaY^2 / n

## Which means that,

## E[Z_bar] = E[X_bar - Y_bar]
##          = E[X_bar] - E[Y_bar]
##          = E[X] - E[Y]
##          = muX - muY
## Var[Z_bar] = Var[X_bar - Y_bar]
##            = Var[X_bar] + Var[Y_bar]
##            = Var[X]/n + Var[Y]/n
##            = sigmaX^2/n + sigmaY^2/n
## sd[Z_bar] = sqrt(sigmaX^2/n + sigmaY^2/n)

## This means that z_bar, where z_bar is a sample from Z_bar = X_bar - Y_bar, is
## an unbiased estimator of mu_diff. It is also minimum variance, but just trust
## me on that one.


## NOTE: Back to the test --- this time for real
## Is there a difference in accuracy between condition 1 and condition 2 during
## phase 1?

## X = RV that generates data for condition 1
## Y = RV that generates data for condition 2
## X and Y are independent

## reload since I've been fooling around for a bit now
rm(list = ls())
f <- '../data/switch_cat_learn/switch_data.csv'
d <- fread(f)

## H0: muX - muY = 0
## H1: muX - muY != 0
mu_Z_bar_null <- 0

## Step 2:
alpha  <- 0.05

## Step 3: Pick a statistic to estimate the parameter in step 1
## Recall that we use z_bar to estimate muX - muY, and z_bar = x_bar - y_bar

## get samples from X ~ condition 1
x <- d[condition==1 & block==1, mean(acc), .(block, subject)][, V1]
nx <- length(x)

## get samples from Y ~ condition 2
y <- d[condition==1 & block==2, mean(acc), .(block, subject)][, V1]
ny <- length(y)

## get sample from X_bar, Y_bar, and Z_bar
x_bar_obs <- mean(x)
y_bar_obs <- mean(y)
z_bar_obs <- x_bar_obs - y_bar_obs

var_z_bar_obs <- var(x)/nx + var(y)/ny
sigma_z_bar_obs <- sqrt(var_Z_bar_obs)

## Because we are estimating the population standard deviation of Z_bar with the
## sample standard deviation, we end up with a t-distribution instead of
## strictly Normal.
t_obs  <- (z_bar_obs - mu_Z_bar_null) / sigma_z_bar_obs

## Compute the p-value
## NOTE: This will take us on another short journey, because to compute the
## p-value, from a t-distribution, you need to know the degrees of freedom of
## the t-dist.

## A t-distribution is standardized, meaning it has mean 0 and variance 1. It
## takes one parameter called the degrees of freedom (df). We have previously
## seen that df=n-1.

## Q: How does that work out here?
## A: Turns out it is kind of a handful

## DF with Equal variances assumed:
## Var[X_bar] == Var[Y_bar]
## df = nx + ny - 2

## DF Equal variances not assumed:
## Var[X_bar] != Var[Y_bar]
## df = ((sd(x)^2)/nx + (sd(y)^2)/ny)^2 /
## ((1/(nx-1))*((sd(x)^2)/nx)^2 + (1/(ny-1))*((sd(y)^2)/ny)^2)

## This last df expression is called the Welch–Satterthwaite equation
## All of this is discussed adequately wiki
## https://en.wikipedia.org/wiki/Student%27s_t-test#Independent_two-sample_t-test


## NOTE: Back to the test. Just need to compute df, p-value, critical value, and
## finally, we can make our decision.

## Here, we will not assume equal variance (brutal!)
df = ((sd(x)^2)/nx + (sd(y)^2)/ny)^2 /
  ((1/(nx-1))*((sd(x)^2)/nx)^2 + (1/(ny-1))*((sd(y)^2)/ny)^2)

## Q: What's up with the "less" and "greater" variable assignments here?
p_obs_less <- pt(t_obs, df, lower.tail = TRUE)
p_obs_greater <- pt(-t_obs, df, lower.tail = FALSE) ## NOTE: unpack the minus sign
t_crit_less <- qt(alpha/2, df, lower.tail = TRUE)
t_crit_greater <- qt(alpha/2, df, lower.tail = FALSE)

if(p_obs_less < alpha/2 | p_obs_greater < alpha/2) {
  print('Reject H0')
} else {
  print('Fail to reject H0')
}

if(t_obs > t_crit_greater | t_obs < t_crit_less) {
  print('Reject H0')
} else {
  print('Fail to reject H0')
}


## Doing this via built-in t.test() function
t.test(x,
       y,
       alternative='two.sided',
       mu=mu_Z_bar_null,
       paired=FALSE,
       var.equal=FALSE,
       conf.level=.95)


## Does this outcome seem reasonable?
xy <- data.table(x, y)
xy <- melt(xy,
           measure.vars = c('x','y'),
           variable.name='condition',
           value.name='acc_mean')
ggplot(xy, aes(x=condition, y=acc_mean)) +
  geom_boxplot(outlier.alpha = 0.0) +
  geom_point(alpha=0.25) +
  theme(aspect.ratio = 1)

xybar <- xy[, .(mean(acc_mean), sd(acc_mean)/sqrt(.N)), .(condition)]
setnames(xybar, 2:3, c('acc_mean', 'acc_err'))
ggplot(xybar, aes(x=condition, y=acc_mean)) +
  geom_bar(stat='identity') +
  geom_errorbar(aes(ymin=acc_mean-acc_err, ymax=acc_mean+acc_err), width=0.2) +
  theme(aspect.ratio = 1)


## NOTE: Comparing two treatments --- repeated measures or paired samples

## Is there a difference between the accuracy during block 2 and block 3 of
## condition 1.

## X = generates data for block 2
## Y = generates data for block 3

## Here, we can't assume X and Y are independent. If a person does well during
## block 1, they are probably more likely to do well during block 2.

## Instead, we approach this situation by computing difference scores:

## | X   | Y   | D     |
## |-----+-----+-------|
## | x1  | y1  | x1-y1 |
## | x2  | y2  | x2-y2 |
## | ... | ... | ...   |
## | xn  | yn  | xn-yn |

## Each row is a measurement from a single subject
## The pairs (X1, Y1)...(Xn, Yn) are independent and identically distributed.
## D -> {d1, d2, ... , dn}
## E[D] = muD
## Var[D] = sigmaD^2

## D_bar = mean({d1, d2, ... , dn})
## E[D_bar] = muD
## Var[D_bar] = sigmaD^2 / n

## Cool, so we're back in familiar waters again...

## Step 1:
## muD = 0
## muD != 0
muD_null <- 0

## Step 2:
alpha <- 0.05

## Step 3:
## get raw observations from X and Y
x_obs <- d[condition==1 & block==2, mean(acc), .(subject)][, V1]
y_obs <- d[condition==1 & block==3, mean(acc), .(subject)][, V1]

## get raw observations from D
d_obs <- x_obs - y_obs

## compute our test statistic (i.e., estimator for the popoulation mean of D)
## Our estimator is the sample mean
## You can think of our experiment as providing a sample of size n=1 from the
## D_bar distribution (i.e., D_bar is our sampling distribution)
d_bar_obs <- mean(d_obs)

## estimate the standard deviation of our sampling distribution (D_bar)
n <- length(d_obs)
d_bar_sigma_obs <- sd(d_obs) / sqrt(n)

## Since we have to estimate the sd of D_bar, we end up with a t-dist.
## Compute t_obs
t_obs <- (d_bar_obs - muD_null) / d_bar_sigma_obs

## Step 4:
## Compute p-values and critical values
df <- n-1
p_obs_lower <- pt(-t_obs, df, lower.tail=TRUE)
p_obs_upper <- pt(t_obs, df, lower.tail=FALSE)
t_crit_lower <- qt(alpha/2, df, lower.tail=TRUE)
t_crit_upper <- qt(alpha/2, df, lower.tail=FALSE)

## Step 5:
if(p_obs_less < alpha/2 | p_obs_greater < alpha/2) {
  print('Reject H0')
} else {
  print('Fail to reject H0')
}

if(t_obs > t_crit_greater | t_obs < t_crit_less) {
  print('Reject H0')
} else {
  print('Fail to reject H0')
}

## Doing this via built-in t.test() function
## Option 1
t.test(x=x_obs,
       y=y_obs,
       alternative='two.sided',
       mu=muD_null,
       paired=TRUE,
       var.equal=FALSE,
       conf.level=1-alpha)

## Doing this via built-in t.test() function
## Option 1
t.test(x=d_obs,
       y=NULL,
       alternative='two.sided',
       mu=muD_null,
       paired=FALSE,
       var.equal=FALSE,
       conf.level=1-alpha)

## Does this result seem plausible?
xy <- data.table(x_obs, y_obs)
xy[, d_obs := x_obs - y_obs]
ggplot(xy, aes(x=1, y=d_obs)) +
  geom_boxplot() +
  geom_hline(yintercept = 0.0, colour='red', linetype=2) +
  theme(aspect.ratio = 1)


## NOTE: Experiment design:
## choosing between paired samples and independent samples:

## |                    | Independent Samples | Paired Samples               |
## |--------------------+---------------------+------------------------------|
## | Degrees of Freedom | nx + ny - 2         | n - 1                        |
## | Variance           | Var[X] + Var[Y]     | Var[X] + Var[Y] - 2*Cov[X,Y] |

## It's a trade-off between more df (independent) vs reduced variance (paired).

## Paired has fewer DF because, by computing difference scores, we are
## effectively halving our sample size.

## However, paired also reduces variance (because Cov[X,Y] is not zero).


## NOTE: I don't think we've covered Cov[X,Y]. Casually, it is what you think of
## as correlation. That is, positive Cov means that if X is big, is Y is also
## likely big. We will likely cover this in more detail next week when we start
## playing around with ANOVA and regression.


## NOTE: Q: Why does more df matter?
## Lets answer this question by examining the t-distribution at different values
## of df

## pick the range of t-values over which to compute the density function
t <- seq(-4, 4, .01)

## create some empty vectors to store results
t_rec <- c()
df_rec <- c()
ft_rec <- c()

## populate rec vectors for df in c(1, 10, 50)
for(df in c(1, 10, 50)) {
  ft <- dt(t, df)
  ft_rec <- c(ft_rec, ft)
  df_rec <- c(df_rec, rep(df, length(t)))
  t_rec <- c(t_rec, t)
}

## store rec vectors in data.table
d <- data.table(df=df_rec, t=t_rec, ft=ft_rec)

## A: Smaller df makes for heavier tails -> larger critical values -> harder to
## reject the null.
ggplot(d, aes(t, ft, colour=as.factor(df))) +
  geom_line()


## TODO: If time permits, pull remaining bits from last lecture
