library('data.table')
library('ggplot2')
library('gtools')

rm(list = ls())

## NOTE: Binomial distribution using R
## Lets consider an experiment in which we flip a coin n times, and count the
## number of heads observed. The coin may be biased or not.

## X ~ binom(n,p)

## n and p are parameters of the distribution


## NOTE:
## dbinom(x, size, prob, log = FALSE)
## pbinom(q, size, prob, lower.tail = TRUE, log.p = FALSE)
## qbinom(p, size, prob, lower.tail = TRUE, log.p = FALSE)
## rbinom(n, size, prob)

## d = density
## p = probability
## q = quantile
## r = random sample

## All of Rs built in distributions use this naming convention


## NOTE:
## define the parameters of X ~ Binom(n,p)
p <- 0.5
n <- 10

## define function arguments common to all variants
size <- n
prob <- p

## define function arguments different between variants
x <- 1:n
density <- dbinom(x, size, prob)
probability <- pbinom(x, size, prob)

pp <- seq(0,1,.2)
quantile <- qbinom(pp, size, prob)

## We want to examine most of these on a single figure, so construct a suitable
## data.table
d_density <- data.table(x, val=density)
d_probability <- data.table(x, val=probability)
d_quantile <- data.table(x=pp, val=quantile)

d_density[, type := 'density']
d_probability[, type := 'probability']
d_quantile[, type := 'quantile']

d <- rbind(d_density, d_probability, d_quantile)

ggplot(d, aes(x, val)) +
  geom_point() +
  geom_segment(aes(x=x, xend=x, y=0, yend=val)) +
  theme(aspect.ratio = 1) +
  facet_wrap(~type, scales='free')

## rbinom draws random samples
size <- n
prob <- p
n_sample <- 1000
sample <- rbinom(n_sample, size, prob)
d <- data.table(sample)
ggplot(d, aes(sample)) +
  geom_histogram()



## NOTE: Use these functions to compute the probabilities of X ~ Binom(n, p)
## set the parameters n and p
n <- 4
p <- 0.5

## compute p(X=0), P(X=1), ..., P(X=5) using dbinom
dbinom(0:n, n, p)

## compare to previous methods
e <- c('H','T')
s <- permutations(length(e), n, e, repeats.allowed = TRUE)
d <- as.data.table(s)
d[, row_ind := 1:.N]
d[, X := sum(.SD == 'H'), .(row_ind)]
dd <- d[, .N, .(X)]
dd[, p := N / d[, .N]] ## This line assumes p = 0.5
dd

## If p != 0.5 then we need to be more elaborate
n <- 4
p <- 0.1

e <- c('H','T')
s <- permutations(length(e), n, e, repeats.allowed = TRUE)
d <- as.data.table(s)
d[, row_ind := 1:.N]
d[, X := sum(.SD == 'H'), .(row_ind)]
d[, p := p]
d[, q := 1-p]
d[, pX := .N * (p^(X) * q^(4-X)), .(X)]
dd <- d[, unique(pX), .(X)]

## Using dbinom is more convenient
dbinom(0:n, n, p)

## NOTE: Compute cumulative probabilities
## lower.tail: logical; if TRUE (default), probabilities are P[X <= x],
## otherwise, P[X > x].

## set parameters n and p
n <- 10
p <- 0.5

## compute p(X > 3)
pbinom(3, n, p, lower.tail=FALSE)

## Compute p(X >= 3):
pbinom(2, n, p, lower.tail=FALSE)

## Compute p(X <= 3)
pbinom(3, n, p, lower.tail=TRUE)

## Compute p(X <= 3) + p(X > 3)
pbinom(3, n, p, lower.tail=TRUE) + pbinom(3, n, p, lower.tail=FALSE)


## NOTE:
## What is the central tendency for the binomial distribution? Hmm... we've seen
## that mean() is a convenient way to compute central tendency for a sample of
## measurements, but does that apply here? This is a different situation. We
## don't have a sample of numbers. Instead, we have a probability distribution
## from which numbers can be sampled. In this context, asking about the central
## tendency of the distribution is like asking what mean(x) would most likely
## give us if we actually performed an experiment (i.e., sampled from the
## distribution). The central tendency of a probability distribution is called
## the expected value of the distribution (or the expected value of the random
## variable, if you prefer. Random variable and distribution are equivalent for
## our purposes here). In other words, if you drew one sample from this
## distribution, what outcome would you most expect? As it turns out, the
## expected value of a RV is defined by the equation:

## E(x) = x1*p(x1) + x2*p(x2) + ... + xn*p(xn)

n <- 10
p <- 0.5
x <- 0:n
y <- dbinom(x, n, p)
d <- data.table(x,y)

exp_val <- d[, sum(x*y)]

## Does this look right? Lets get a feel for it by computing the expected value
## for a bunch of different n and p.
n_levels <- c(5, 15, 30)
p_levels <- c(.1, .5, .9)

## Start with the same code we previously used for multiple n and p
n_rec <-  c()
p_rec <- c()
x_rec <- c()
p_binom_rec <- c()
for(n in n_levels) {
  for(p in p_levels) {
    n_rec <- c(n_rec, rep(n, n+1))
    p_rec <- c(p_rec, rep(p, n+1))
    x_rec <- c(x_rec, 0:n)
    p_binom_rec <- c(p_binom_rec, dbinom(0:n, n, p))
  }
}

d <- data.table(n=n_rec, p=p_rec, x=x_rec, p_binom = p_binom_rec)

## Add exp_val col to this data.table
d[, exp_val := sum(x*p_binom), .(n,p)]

## Plot the expected values to see if they seem plausible
ggplot(d, aes(x=x, y=p_binom)) +
  geom_point() +
  geom_segment(aes(xend=x, yend=0)) +
  xlab('X = number successes in n Bernoulli trials') +
  ylab('Probability') +
  ## theme(aspect.ratio = 1) +
  facet_wrap(~n*p) +
  geom_vline(aes(xintercept=exp_val, colour='red'))


## NOTE: What is the spread of the binomial distribution
## Variance - A measure of spread
## Population Variance = The expected squared deviation from the mean
## Var(X) = E((X - E(X))^2)
## Var(X) = sum(x^2*p_x) - mu^2

## Add a var col to the data.table we just used in the previous section
d[, pop_var := sum((x^2)*p_binom) - exp_val^2, .(n,p)]
d[, pop_sd := sqrt(pop_var)]

## add +/- sd to the plot to check if plausible
ggplot(d, aes(x=x, y=p_binom)) +
  geom_point() +
  geom_segment(aes(xend=x, yend=0)) +
  xlab('X = number successes in n Bernoulli trials') +
  ylab('Probability') +
  ## theme(aspect.ratio = 1) +
  facet_wrap(~n*p) +
  geom_vline(aes(xintercept=exp_val, colour='red')) +
  geom_segment(aes(
    x=exp_val-pop_sd,
    xend=exp_val+pop_sd,
    y=0.5,
    yend=0.5,
    colour='red'))


## NOTE: population mean and population variance (i.e., expected value stuff),
## derived analytically.

## Consider the special case of n=1, with probability of success (S), p, and
## probability of failure (F), q.

## Let X ~ binomial(n,p)

## Since n=1, there are only possible outcomes: [0, 1]

## E(X) = 0*q + 1*p = p
## Var(X) = 0^2*q + 1*^2*p - p^2 = p - p^2 = p*(1-p) = p*q

## The trick now is to realize that for n > 1, we can simply think of the
## resulting binomial distribution as the sum of the n=1 binomial we just looked
## at.

## X = X1 + X2 + ... + Xn

## E(X) = E(X1 + X2 + ... + Xn)
##      = E(X1) + E(X2) + ... + E(Xn)
##      = p + p + ... + p
##      = n*p

## Var(X) = Var(X1 + X2 + ... + Xn)
##        = Var(X1) + Var(X2) + ... + Var(Xn)
##        = p*q + p*q + ... + p*q
##        = n*p*q
##        = n*p*(1-p)

## E(X) = n*p
## Var(X) = n*p*(1-p)


# NOTE: Do the analytically derived pop_mean and pop_var line up with our
# computed versions?

## use the data.table that we've been using to check
## If all is well, then the difference between the two should be zero
d[, exp_val - n*p]
d[, pop_var - n*p*(1-p)]

## All looks fine. I'm pretty sure what you're seeing above is rounding error.


## NOTE: Lets carefully explore the relationship between sample and population
n <- 100000
p <- 0.5

n_sample_size <- 50

## Draw random samples from X ~ Binom(n,P) in many different experiments
r_binom_rec <- c()
exp_rec <- c()
for(i in 1:9) {
  r_binom_rec <- c(r_binom_rec, rbinom(n_sample_size, n, p))
  exp_rec <- c(exp_rec, rep(i, n_sample_size))
}

d <- data.table(exp=exp_rec, val = r_binom_rec)

## Add pop_mean to this data.table
d[, pop_mean := n*p]

## Add sample_mean to this data.table
d[, sample_mean := mean(val), .(exp)]

ggplot(d, aes(val)) +
  geom_histogram(aes(y=..density..), bins=10) +
  xlab('X = number successes in n Bernoulli trials') +
  ylab('Probability Estimate') +
  ## theme(aspect.ratio = 1) +
  facet_wrap(~exp, ncol=3) +
  geom_vline(aes(xintercept=pop_mean, colour='red')) +
  geom_vline(aes(xintercept=sample_mean, colour='blue'))


## NOTE:
## Suppose we want to determine if a coin is fair or not. Further suppose that
## we are willing to flip it 100 times before making our decision.

## X ~ Binom(n, p)

## set the n parameter of the distribution
n = 100

## Here, I'm setting up a situation in which the parameter p will be one of .4,
## .5, or .6. Unless we cheat and print it out, we will not which it is.
p = sample(c(.4,.5,.6), 1, replace=TRUE)

## perform the experiment (i.e., flip it n times) and record the results
exp_results  <- rbinom(1, n, p)

## You have your results. What do you think? Is the coin fair or not? T0 answer
## this, we will use Null Hypothesis Significance Testing (NHST)


## NOTE: NHST

########################################
## Step 1: State the Hypotheses.
########################################

## The Null Hypothesis (H0): p = 0.5
## The Alternative Hypothesis (H1): p > 0.5

########################################
## Step 2: Define a statistic that estimates the parameters used in step 1
########################################

## Here, we use relative frequency of heads as an estimate for P(heads)
p_estimate = exp_results / n

########################################
## Step 3: What is the probability of observing our data (or data more extreme)
########################################

## than what we observed) under the assumption that the null hypothesis (H0) is
## true?

## define p assuming H0 is true
p_H0 <- .5

## compute P(our data or more extreme data | H0)
## Recall: lower.tail:
## if TRUE:  P[X <= x],
## if FALSE: P[X > x].
p_data_given_H0 <- pbinom(exp_results-1, n, p_H0, lower.tail=FALSE)

## Q: What's up with the exp_results -1 in the first argument above?
## A: ???

## In statistical hypothesis testing, the p-value or probability value or
## asymptotic significance is the probability for a given statistical model
## that, when the null hypothesis is true, the statistical summary (such as the
## sample mean difference between two compared groups) would be the same as or
## of greater magnitude than the actual observed results.

########################################
## Step 3: Visually
########################################

## Lets also visually examine P(data|H0)

## NOTE: plot the null distribution (mass function) and shade the region
## corresponding to the probability of observing our results or more extreme
## results.
x <- 1:100
p <- dbinom(x, n, p_H0)
d <- data.table(x,p)

## On our way to figure, but there's some cool stuff we can do with just the
## data.table
p_value <- d[exp_results:.N, sum(p)]

ggplot(d, aes(x,p)) +
  geom_rect(aes(
    xmin=exp_results,
    xmax=Inf,
    ymin=0,
    ymax=Inf),
    fill='gray',
    alpha=0.1) +
  geom_point() +
  geom_segment(aes(x=x, xend=x, y=0, yend=p)) +
  geom_segment(x=exp_results, xend=exp_results, y=0, yend=Inf, colour='red') +
  xlab('Number of successes in n Bernoulli trials') +
  theme(aspect.ratio = 1)

## NOTE: I tend to think in terms of the density (or in this case mass)
## function, but also good to think with the cumulative probability function.
x <- 1:100
p <- pbinom(x, n, p_H0, lower.tail=FALSE)
d <- data.table(x,p)

## On our way to figure, but there's some cool stuff we can do with just the
## data.table
p_value <- d[x==exp_results-1, p]

ggplot(d, aes(x,p)) +
  geom_point() +
  geom_segment(aes(x=x, xend=x, y=0, yend=p)) +
  geom_segment(x=exp_results, xend=exp_results, y=0, yend=Inf, colour='red') +
  xlab('Number of successes in n Bernoulli trials') +
  theme(aspect.ratio = 1)

## NOTE: For kicks, put pmf and cdf on same plot
x <- 1:100
pmf <- dbinom(x, n, p_H0)
cdf <- pbinom(x, n, p_H0, lower.tail=FALSE)
d <- data.table(x,pmf,cdf)

## p-value from pmf and cdf
d[x==exp_results-1, cdf]
d[exp_results:.N, sum(pmf)]

## plot pmf and cdf together
dd <- melt(d, id.vars='x')
ggplot(dd, aes(x,value)) +
  geom_point() +
  geom_segment(aes(x=x, xend=x, y=0, yend=value)) +
  geom_segment(x=exp_results, xend=exp_results, y=0, yend=Inf, colour='red') +
  xlab('Number of successes in n Bernoulli trials') +
  theme(aspect.ratio = 1) +
  facet_wrap(~variable, scales='free')


########################################
## Step 4: How confident do you need be?
########################################

## What are the consequences if you decide wrong? In psychology and many other
## fields, we are often willing to be wrong 5 times out of a 100. It is
## convention to call this rate alpha.

alpha = 0.05

## Alpha goes by a couple names:
## Type I Error
## False Positive Rate

########################################
## Step 5: Make your decision
########################################

## If P(p_data_given_H0) is small -> reject H0
## If P(p_data_given_H0) is large -> fail to reject H0

## So what is large and what is small?
## The threshold for small is given by alpha.

## If P(p_data_given_H0) < alpha -> reject H0
## If P(p_data_given_H0) >= alpha -> fail to reject H0

if(p_data_given_H0 < alpha) {
  print('Reject H0:')
} else {
  print('Failed to reject H0:')
}

########################################

## NOTE: failing to reject H0 is not the same thing as proving or supporting H0.
## That is a logical fallacy. Don't make science sad by committing this error.

########################################

########################################

## NOTE: Of course, R will handle all this for you.
binom.test(exp_results, n=n, p=p_H0, alternative='greater', conf.level=0.05)

########################################

# NOTE: Pickup here
## Suppose we have a board game that depends on the roll of one die and attaches
## special importance to rolling a 6.

## Success (S) = roll a 6
## Failure (F) = roll anything that's not a 6

## In a particular game, the die is rolled 235 times, and 6 comes up 51 times.
## Is the proportion of 6s significantly higher than would be expected by
## chance, on the null hypothesis of a fair die?

## step 1:
## H0: p = 1/6
## H1: p > 1/6
n <- 235
p_H0 <- 1/6

## step 2:
## X ~ binom(n, p)
x <- 51

## step 3: P(x|p_H0
p_val <- pbinom(x-1, n, p_H0, lower.tail=FALSE)

## step 4: state our required confidence level
alpha = 0.05

## step 5: make a decision
if(p_val < alpha) {
  print('Reject H0:')
} else {
  print('Failed to reject H0:')
}


## NOTE: plot the null distribution (mass function) and shade the region
## corresponding to the probability of observing our results or more extreme
## results.
x_obs <- 51
x <- 0:n
p <- dbinom(x, n, p_H0)
d <- data.table(x,p)

## On our way to figure, but there's some cool stuff we can do with just the
## data.table
p_val <- d[x_obs:.N, sum(p)]

## verify same as above p_val
d[x_obs:.N, sum(p)]
pbinom(x_obs-1, n, p_H0, lower.tail=FALSE)

## plot pmf
ggplot(d, aes(x,p)) +
  geom_rect(aes(
    xmin=x_obs,
    xmax=Inf,
    ymin=0,
    ymax=Inf),
    fill='gray',
    alpha=0.1) +
  geom_point() +
  geom_segment(aes(x=x, xend=x, y=0, yend=p)) +
  geom_segment(x=x_obs, xend=x_obs, y=0, yend=Inf, colour='red') +
  xlab('Number of successes in n Bernoulli trials') +
  theme(aspect.ratio = 1)

## NOTE: do it via cdf
x <- 0:n
p <- pbinom(x, n, p_H0, lower.tail=FALSE)
d <- data.table(x,p)

## On our way to figure, but there's some cool stuff we can do with just the
## data.table
p_value <- d[x==exp_results-1, p]

## verify same as above p_val
d[x==x_obs-1, p]
d[x_obs:.N, sum(p)]
pbinom(x-1, n, p_H0, lower.tail=FALSE)

## plot cdf
ggplot(d, aes(x,p)) +
  geom_point() +
  geom_segment(aes(x=x, xend=x, y=0, yend=p)) +
  geom_segment(x=exp_results, xend=exp_results, y=0, yend=Inf, colour='red') +
  xlab('Number of successes in n Bernoulli trials') +
  theme(aspect.ratio = 1)


## NOTE: do it all in one line with the magic of R
binom.test(51,235,(1/6),alternative="greater", conf.level=0.95)


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

## NOTE:
## P(type I error) = alpha = total mass of H0 pmf in the rejection region
## Power = P(correctly rejecting H0) = total mass of H1 pmf in rejection region

## TODO: clear up rejection region with one more simpler plot before the below
## example?

## NOTE:
n <- 235
p_H0 <- 1/6
p_H1 <- 3/12

x_obs <- 51

x <- 0:n
p_H0 <- dbinom(x, n, p_H0)
p_H1 <- dbinom(x, n, p_H1)
d <- data.table(x, p_H0, p_H1)

dd <- melt(d, id.vars='x')

ggplot(dd, aes(x, value, colour=variable)) +
  ## geom_rect(aes(
  ##   xmin=x_obs,
  ##   xmax=Inf,
  ##   ymin=0,
  ##   ymax=Inf,
  ##   fill='red',
  ##   alpha=0.1)
  ##   ) +
  geom_point() +
  geom_segment(aes(x=x, xend=x, y=0, yend=value)) +
  geom_segment(x=x_obs, xend=x_obs, y=0, yend=Inf, colour='black') +
  xlab('Number of successes in n Bernoulli trials') +
  theme(aspect.ratio = 1)
