library('data.table')
library('ggplot2')
library('gtools')

rm(list = ls())


## NOTE:
## A store manager uses extensive sales records to assess the probability
## distributions for:

## X = number of units of brand A sold in a week
## Y = number of units of brand B sold in a week

## These distributions are given by the following table:

## | Brand A | Value x          |   0 |   1 |   2 |   3 |   4 |   5 |
## | Brand A | Probability f(x) |  .1 |  .1 |  .2 |  .3 |  .2 |  .1 |
## |---------+------------------+-----+-----+-----+-----+-----+-----|
## | Brand B | Value y          |   0 |   1 |   2 |   3 |   4 |   5 |
## | brand B | Probability f(y) | .23 | .48 | .29 | 0.0 | 0.0 | 0.0 |

## Plot probability distributions of X and Y:

## We begin, as usual, by converting table to data.table
value <- c(0,1,2,3,4,5)
p_x <- c(.1,.1,.2,.3,.2,.1)
p_y <- c(.23,.48,.29, 0.0, 0.0, 0.0)
d <- data.table(value, p_x, p_y)

## This data.table is in "wide format"
## Convert to "long format"
dd <- melt(d, id.vars=1, measure.vars=2:3, value.name='p')

## Plot the distributions
ggplot(dd, aes(x=value, y=p)) +
  geom_point() +
  geom_segment(aes(xend=value, yend=0)) +
  facet_wrap(~variable) +
  theme(aspect.ratio = 1)

## NOTE: Expected value and central tendency of a RV
## Brand A has more spread (variance) than brand B, and its central tendency
## (mean) appears to be something like 2 or 3, whereas brand B has a mean of 1
## or so.

## Is there anything odd or different about the idea of central tendency and
## spread here?

## The key difference I'm hinting at is that in the past, we have applied these
## concepts to samples (i.e., collections of numbers obtained after an
## experiment has been performed). Remember that we called the resulting numbers
## descriptive statistics.

## We have even used the numbers in the sample that we obtained or were given to
## estimate the probability of particular events happening. We used the relative frequency of an event as an estimate of the probability.

## We also saw that relative frequency is a great estimate of true probability
## only if the sample size is very large.

## Here, we are not given a sample, nor are we trying to estimate the
## probability of some event. Rather, we are given exact probabilities. That is,
## we are given exact information about a population. This knowledge tells us
## what to expect if we were to perform an experiment in which we sampled from
## this population.

## Turns out that central tendency and measures of spread are also important
## descriptions of population level knowledge / probability distributions. As we
## will see, they are computed a bit differently, but they are nevertheless
## closely related to their descriptive statistic cousins.

## In particular, population mean and population variance tell you what to most expect from an experiment performed in which you sample from the population.

## On the other hand, sample mean and sample variance are estimates for the
## population mean and population variance for the underlying population
## probability distribution.

## sample = some numbers that came from an experiment
## population = the distribution that generated those numbers

## Okay, for a population probability distribution, the mean of a distribution
## is also called the "expected value" of a distribution. It is computed:

## E(X) = population mean = x1*p(x1) + x2*p(x2) + ... + xn*p(xn)

## Compute expected value of X and Y
E_x <- 0*.1 + 1*.1 + 2*.2 + 3*.3 + 4*.2 + 5*.1
E_y <- 0*.23 + 1*.48 + 2*.29 + 3*0 + 4*0 + 5*0

## We can do this more efficiently by using the data.table
dd[, sum(value*p), .(variable)]
dd[, pop_mean := sum(value*p), .(variable)]

## Now, suppose that a profit of $50 is realized on each unit sold and that the
## weekly fixed cost is $20. What is the expected net profit?

## Expected net profit:
## (profit per sale) * (expected number of units sold) - (weekly fixed cost)
dd[, 50*sum(value * p) - 20, .(variable)]
dd[, 50*pop_mean - 20, .(variable)]


## NOTE: Variance - A measure of spread
## Population Variance = The expected squared deviation from the mean
## E(X) = x1*p(x1) + x2*p(x2) + ... + xn*p(xn)
## Var(X) = E((X - E(X))^2)

## Y = (X - E(X))^2
## Var(X) = E(Y)

## Let E(X) = mu
## Then:
## Var(X) = E((X - mu)^2)
## Var(X) = E(X^2 - 2*X*mu + mu^2)
## Var(X) = E(X^2) - E(2*X*mu) + E(mu^2)
## Var(X) = E(X^2) - 2*mu*E(X) + mu^2
## Var(X) = E(X^2) - 2*mu^2 + mu^2
## Var(X) = E(X^2) - mu^2
## Var(X) = sum(x^2*p_x) - mu^2

## The sample mean is an estimate for the population mean, and the sample
## variance is an estimate for the population variance.

## Comptue the (population) variance and standard deviation of X and Y from the
## above example.

## Lets take an iterative approach. Start by computing the expected value (mean)
dd[, pop_mean := sum(value*p), .(variable)]

## Now compute variance
dd[, pop_var := sum((value^2)*p) - pop_mean^2, .(variable)]

## Compute standard deviation (sd = sqrt(var))
dd[, pop_sd := sqrt(pop_var)]

## Plot the distributions with EV and SD added
ggplot(dd, aes(x=value, y=p)) +
  geom_point() +
  geom_segment(aes(xend=value, yend=0)) +
  facet_wrap(~variable) +
  theme(aspect.ratio = 1) +
  geom_vline(aes(xintercept=pop_mean, colour='red')) +
  geom_segment(aes(
    x=pop_mean-pop_sd,
    xend=pop_mean+pop_sd,
    y=0.5,
    yend=0.5,
    colour='red'))


## NOTE:
## A large store places its last 15 clock radios in a clearance sale. Unknown to
## anyone, 5 of these radios are defective. If a customer tests 3 different
## clock radios selected at random, what is the distribution of the following
## random variable X:

## X = number of defective radios in the sample

## Q: Is our sample space defined by permutations or combinations?
## A: X is merely concerned with the number of defective, not the order in which
## those defectives occur (GGB == GBG == BGG etc.), so we are dealing with
## combinations.

## what is the sample space
## Below, note the use of the 'set' argument to combinations
e <- c(rep('good',10), rep('bad',5))
s <- combinations(length(e), 3, e, set=FALSE, repeats.allowed=FALSE)
d <- as.data.table(s)

## Quick check: we know there are n choose k different ways to combine 3 items
## drawn simultaneously from a population of 15
choose(15, 3)
choose(15, 3) == d[, .N]

## Define X from the sample space
## X = number of defective radios in the sample
d[, row_ind := 1:.N]
d[, X := sum(.SD == 'bad'), .(row_ind)]

## Quick check: it is clear if you think about it that there are only 4 distinct
## possible outcomes 0, 1, 2, and 3. Is that what we ended up with?
d[, unique(X)]

## Define a data.table to contain the random variable X and it's probability
## distribution
dd <- d[, .N, .(X)]
dd[, p := N / d[, .N]]

## plot the prob dist (PMF)
ggplot(dd, aes(x=X, y=p)) +
  geom_point() +
  geom_segment(aes(xend=X, yend=0)) +
  xlab('X = number of defective radios in the sample') +
  ylab('Probability') +
  theme(aspect.ratio = 1)

## We can also get the probability distribution using math:

## The probability of selecting 0 bad radios is given by number of ways to
## select 3 good radios divided by the number of ways to select 3 radios from
## the whole population.
p0 <- choose(10, 3) / choose(15, 3)

## The probability of selecting 1 bad radio is given by the number of ways to
## select 2 good and 1 bad divded by the number of ways to select 3 from the
## whole population.
p1 <- (choose(10,2) * choose(5,1)) / choose(15, 3)

## so on and so forth
p2 <- (choose(10,1) * choose(5,2)) / choose(15, 3)
p3 <- choose(5,3) / choose(15, 3)

## The probability distribution is obtained simply by collecting the individual
## probabilities
p <- c(p0,p1,p2,p3)


## NOTE:
## Suppose that two products A and B are judged by four consumers who then
## express a preference for A or B. We denote each outcome as a four element
## string, with each element indicating the preference of a consumer (e.g., ABAB
## denotes an outcome where the first and third consumer prefer product A and
## the second and fourth consumer prefer product B).

## Get every possible outcome (i.e., define the sample space):
## Do it using the gtools library
e <- c('A', 'B')
possible_permutations <- permutations(length(e), 4, e, repeats.allowed=TRUE)
possible_combinations <- combinations(length(e), 4, e, repeats.allowed=TRUE)

## Does the size of these vectors make sense?
## Each consumer can choose A or B, so each consumer produces two possible
## outcomes. Recall that the number of unique ways to permute n items is:
## n*(n-1)*(n-2)*...*(n-r+1) = (n!) / (n-r)!
## It's definitely a bit silly to apply this here, but here it is:
n <- 2
r <- 1
num_outcomes_per_consumer <- factorial(n) / factorial(n-r)

## Okay, all good there. Now translate this to the entire experiment. For each
## choice the first consumer makes, the second consumer can make two choices, so
## the total number of permutations generated by the first two consumers is:
num_outcomes_per_consumer * num_outcomes_per_consumer

## Similar logic holds for the remaining consumers. We end up with:
(num_outcomes_per_consumer)^4

## How many combinations should we expect from this experiment? Since order
## doesn't matter for combinations, we are really looking at the number of As
## and Bs selected. From that perspective, using the magic of your brain, it is
## clear that there are only five possible outcomes. E.g., everything from 0 A
## outcomes to 4 A outcomes.

## What's the different between permutations and combinations?
## permutations: different orders count as different outcomes
## combinations: different orders count as the same outcomes
## Which is the appropriate definition of the sample space here?
## One way of doing this is to construct a simple test case:
## Is ABAB different outcome then BABA?
## Both have 2 As and 2 Bs, but since they correspond to different consumers, we
## should treat them as different outcomes. This means that our sample space is defined by permutations.
possible_outcomes <- possible_permutations

## Review: Recall that the repeats.allowed argument determines whether sampling
## is with (TRUE) or without (FALSE) replacement. We can get a good feel for
## this by trying it out here. We should get an ERROR if we try to sample
## without replacement 4 items from a 2 item population.
e <- c('A', 'B')
possible_permutations <- permutations(length(e), 4, e, repeats.allowed=FALSE)
possible_combinations <- combinations(length(e), 4, e, repeats.allowed=FALSE)

## convert to data.table form, because everything is better that way
d <- as.data.table(possible_outcomes)

## add a column to code for row index. This will come in handy if I want to
## perform row-wise computations
d[, row_ind := 1:.N]

## Now suppose that the products are alike in quality and that the consumers
## express their preferences independently of each other. Then the 16 simple
## events in the sample space are equally likely, and each has a probability of
## 1/16. Let a random variable be defined as:

## X = number of persons preferring A over B

## Add X to our data.table
d[, X := sum(.SD == 'A'), .(row_ind)]

## write down the probability distribution of X in table form:
table(d[, X])

## That last line was close, but not quite there. The issue is that table() just
## ocunts up the number of occurances of each unique outcome. We need to convert
## these counts into probabilities. To do this, we must divide each outcome by
## the total number of possible outcomes.
p <- table(d[, X]) / d[, .N]

## tables are a fine approach but for the methods we've been developing, lets
## try to keep everything in data.tables. We should do this not only because
## data.tables allow us to operate on our data more effectively, but also
## because ggplot requires them to plot for us.
dd <- d[, .N, .(X)]
dd[, p := N / d[, .N]]

## Okay, we've added p (probability) to the data.table in a way the probably
## makes good sense intuitively. Is the p we just added an estimate for the true
## probability?

## Find p(X >= 2)
dd[X >= 2, sum(p)]

## Find p(1 <= X <= 3)
dd[X >= 1 & X <= 3, sum(p)]

## Find p(X < 2)
dd[X<2, sum(p)]

## Find p(X > 4)
dd[X>4, sum(p)]

## Find p(X <= 4)
dd[X<=4, sum(p)]

dd[X < 4]

## plot the probability distribution
## Note that when X is discrete, this is called the probability mass function
## and commonly abbreviated PMF
ggplot(dd, aes(x=X, y=p)) +
  geom_point() +
  geom_segment(aes(xend=X, yend=0)) +
  xlab('X = number of A preferences') +
  ylab('Probability') +
  theme(aspect.ratio = 1)


## Q: What is another name for the expected value of X?
## A: population mean

## Q: What is another name for the expected value of the squared deviation from X?
## A: population variance

## Add population mean and variance to data.table and plot
dd[, pop_mean := sum(X*p)]

## Now compute variance
dd[, pop_var := sum((X^2)*p) - pop_mean^2]

## Compute standard deviation (sd = sqrt(var))
dd[, pop_sd := sqrt(pop_var)]

## Plot the distributions with EV and SD added
ggplot(dd, aes(x=X, y=p)) +
  geom_point() +
  geom_segment(aes(xend=X, yend=0)) +
  theme(aspect.ratio = 1) +
  geom_vline(aes(xintercept=pop_mean, colour='red')) +
  geom_segment(aes(
    x=pop_mean-pop_sd,
    xend=pop_mean+pop_sd,
    y=0.5,
    yend=0.5,
    colour='red')) +
  xlab('X = number of A preferences') +
  ylab('Probability')


## NOTE: Bernoulli Random Variable
## The kind of dichotomous outcomes that we just saw in the above example, and
## more commonly see when we think about flipping a coin, is often formally
## defined as Bernoulli trial. To be a Bernoulli trial, a few conditions must be
## met:
## (1) Each trial yields one of the two outcomes technically called a success (S)
## and failure (F).
## (2) For each trial, the probability of success P(S) is the same and is
## denoted by p = P(S). The probability of failure is then q = P(F) = 1 - P(S)
## for each trial.
## (3) Trials are independent. The probability of success in a trial does not
## change given any amount of information about the outcomes of other trials.

## For example: Coin toss

## As we will next, the Bernoulli distribution is a special case of the binomial
## distribution where a single experiment / trial is conducted (n=1).

## NOTE: Binomial distribution
## Consider a fixed number n of Bernoulli trials conducted with success
## probability p and failure probability q in each trial. Further consider the
## random variable:

## X = the sum of the number of Successes from n Bernoulli trials

## Then X is called a binomial distribution with n trials and success
## probability p. The binomial distribution is completely defined by two
## parameters, n and p, and we can write:

## X ~ Binomial(n,p)

## ~ means "is distributed as"


## NOTE:
## Consider the example above, where four consumers prefer either product A or
## B. Define the random variable:

## X = the number of these four consumers preferring product A.

## What is the sample space?
## We have already seen that this is given by:
e <- c('A','B')
s <- permutations(length(e), 4, e, repeats.allowed = TRUE)

## As above, convert to data.table and add the RV X, etc.
d <- as.data.table(s)
d[, row_ind := 1:.N]
d[, X := sum(.SD == 'A'), .(row_ind)]

## Lets think about where these p-values come from.
## Consider each possible outcome of X in turn:

## First define p and q
p <- .5
q <- .5

## X = 0
d[X==0]

## P(X = 0)
p0 <- d[X==0, .N * (q^4)]

## The core insight here is that the probability of a particular outcome is the
## product of the p and q sequence (because trials are independent of each
## other) times the number of ways that outcome can happen.

## X = 1
d[X==1]

## P(X = 1)
p1 <- d[X==1, .N * (p * q^3)]

## X = 2
d[X==2]

## P(X = 2)
p2 <- d[X==2, .N * (p^2 * q^2)]

## X = 3
d[X==3]

## P(X = 3)
p3 <- d[X==3, .N * (p^3 * q^1)]

## X = 4
d[X==4]

## P(X = 4)
p4 <- d[X==4, .N * (p^4)]

## These should all look familiar:
c(p0, p1, p2, p3, p4)

## Lets do this another way:
d[, p := 0.5]
d[, q := 1-p]
d[, pX := .N * (p^(X) * q^(4-X)), .(X)]

## Again, these should all look familiar:
dd <- d[, .N, .(X)]
dd[, p := N / d[, .N]]
dd

d[, unique(pX), .(X)]
c(p0, p1, p2, p3, p4)

## Lets do it using a more generic method, so that it is easy to explore what
## happens when p != q
d[, p := 0.1]
d[, q := 1-p]
d[, pX := .N * (p^(X) * q^(4-X)), .(X)]
dd <- d[, unique(pX), .(X)]
ggplot(dd, aes(X, V1)) +
  geom_point() +
  geom_segment(aes(xend=X, yend=0)) +
  theme(aspect.ratio = 1)

## By playing around with different values of p, you can see that the shape of
## the binomial distribution is skewed towards p if p is large and away from p
## if p is small.


## NOTE: binomial distribution using R
## Lets consider an experiment in which we flip a coin n times, and count the
## number of heads observed. The coin may be biased or not.

## X ~ binom(n,p)

## n and p are parameters of the distribution

## To begin, lets explore n and p to the example above
n <- 4
p <- 0.5

## compute p(X=0), P(X=1), ..., P(X=5) and compare to previous method
dbinom(0:4, n, p)

e <- c('A','B')
s <- permutations(length(e), 4, e, repeats.allowed = TRUE)
d <- as.data.table(s)
d[, row_ind := 1:.N]
d[, X := sum(.SD == 'A'), .(row_ind)]

dd <- d[, .N, .(X)]
dd[, p := N / d[, .N]]
dd

## lower.tail: logical; if TRUE (default), probabilities are P[X <= x],
## otherwise, P[X > x].

## compute p(X > 3)
pbinom(3, n, p, lower.tail=FALSE)

## Compute p(X >= 3):
pbinom(2, n, p, lower.tail=FALSE)

## Compute p(X <= 3)
pbinom(3, n, p, lower.tail=TRUE)

## Compute p(X <= 3) + p(X > 3)
pbinom(3, n, p, lower.tail=TRUE) + pbinom(3, n, p, lower.tail=FALSE)


## NOTE: plot binomial probability distribution
n <- 10
p <- 0.5
x <- 0:n
y <- dbinom(x, n, p)
d <- data.table(x,y)
ggplot(d, aes(x=x,y=y)) +
  geom_point() + # we use point because it's a discrete distribution
  geom_segment(aes(xend=x, yend=0)) +
  xlab('X = number successes in n Bernoulli trials') +
  ylab('Probability') +
  theme(aspect.ratio = 1)

## Lets more formally examine the of effect different values of n and p
n_levels <- c(5, 15, 30)
p_levels <- c(.1, .5, .9)

## We've used this programming construct before, but it can still be tricky.
## Take it slow.
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

ggplot(d, aes(x=x, y=p_binom)) +
  geom_point() +
  geom_segment(aes(xend=x, yend=0)) +
  xlab('X = number successes in n Bernoulli trials') +
  ylab('Probability') +
  ## theme(aspect.ratio = 1) +
  facet_wrap(~n*p) ## this might be a cool new use of facet_wrap


## NOTE: Sample from a binomial distribution
## draw a sample from the above geometric distribution and plot it with a
## histogram
n <- 10000
p <- 0.5
x <- 0:n
sample <- rbinom(x, n, p)
d <- data.table(sample)
ggplot(d, aes(sample)) +
  geom_histogram(aes(y=..density..)) +
  theme(aspect.ratio = 1)

## TODO: Use the last few plots to emphasize the difference between population
## and sample.


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


## NOTE: Geometric distribution
## The chance that monkeys will have a positive (successful) reaction during a
## trial of an experiment is 1/3. Trials are performed until there is a success.
## Define a random variable X:

## X = number of trials performed

## What is the probability distribution of X?

## Let S denote success and F denote failure
## Then the sample space is:
## S = {S, SF, SSF, SSSF, ...}

## You can see that it's impossible to make a complete list of all the values
## and their probabilities. However, this is a well studied problem, and it
## turns out that there is an formula that will give p(x) for any x:
## p(x) = (1/3)*(2/3)^(x-1)
## Also, good ol' R has it built right in:
## pgeom()
## https://stat.ethz.ch/R-manual/R-devel/library/stats/html/Geometric.html
## https://en.wikipedia.org/wiki/Geometric_distribution

## The geometric distribution is defined by one parameter:
## The probability of success
p_success  <- 1/3

## compute p(X > 3)
## Note that pgeom uses "0" to index the first success
pgeom(2, p_success, lower.tail=FALSE)

## Compute p(X >= 3):
pgeom(1, p_success, lower.tail=FALSE)

## Compute p(X <= 3)
pgeom(2, p_success, lower.tail=TRUE)

## Compute p(X <= 3) + p(X > 3)
pgeom(2, p_success, lower.tail=TRUE) + pgeom(2, p_success, lower.tail=FALSE)


## NOTE: plot geometric probability distribution
x <- 0:10
y <- dgeom(x, 1/3)
d <- data.table(x,y)
ggplot(d, aes(x=x,y=y)) +
  geom_point() + # we use point because it's a discrete distribution
  xlab('X = number trials until first success') +
  ylab('Probability') +
  theme(aspect.ratio = 1)


## NOTE: plot samples from a geometric distribution
## draw a sample from the above geometric distribution and plot it with a
## histogram
n <- 1000
sample <- rgeom(n, 1/3)
d <- data.table(sample)
b <- seq(0.1,10,1)
ggplot(d, aes(sample)) +
  geom_histogram(aes(y=..density..),breaks=b) +
  theme(aspect.ratio = 1)

## Might out nicely here to use a bar graph (I'm guessing because I had to
## fiddle around with the breaks argument of histogram to get it to look good,
## and since the geometric distribution is discrete, discrete bars are
## appropriate).
n <- 1000
sample <- rgeom(n, 1/3)
d <- data.table(sample)
dd <- data.table(table(d))
setnames(dd, 'd', 'x')
dd[, p_est := N / n] ## Use relative frequency to estimate probability
dd[, x := as.integer(x)]
ggplot(dd, aes(x=x, y=p_est)) +
  geom_bar(stat='identity') +
  theme(aspect.ratio = 1)

## TODO: Use the last few plots to emphasize the difference between population
## and sample.


## NOTE: plot different distributions for different parameter values
p1 <- .2
p2 <- .5
p3 <- .8
x <- 0:10
y1 <- dgeom(x, p1)
y2 <- dgeom(x, p2)
y3 <- dgeom(x, p3)
p <- rep(c(p1,p2,p3), each=length(x))
d <- data.table(x,y1,y2,y3)
dd <- melt(d, measure.vars = c('y1', 'y2', 'y3'))
dd[, p := p]

ggplot(dd, aes(x=x,y=value)) +
  geom_point() +
  geom_segment(aes(xend=x, yend=0)) +
  xlab('X = number trials until first success') +
  ylab('Probability') +
  theme(aspect.ratio = 1) +
  facet_wrap(~p)


## NOTE:
## Compute the expected value of the geometric distributions that we just
## plotted above. For the geometric distribution, x can assume any value from
## zero to infinity, but for now, assume x only ranges from 0 to 10.
## Recall: E(x) = x1*p(x1) + x2*p(x2) + ... + xn*p(xn)
dd[, pop_mean := sum(x*value), .(p)]

## add pop_mean to plot to see if they look plausible
ggplot(dd, aes(x=x,y=value)) +
  geom_point() +
  geom_segment(aes(xend=x, yend=0)) +
  xlab('X = number trials until first success') +
  ylab('Probability') +
  theme(aspect.ratio = 1) +
  facet_wrap(~p) +
  geom_vline(aes(xintercept=pop_mean, colour='red'))

## They look plausible, but it turns out it can be proven that if X ~ geometric
## with probability of success of p, then E(X) = (1-p)/p. Verify that is true in our
## little example:
dd[, pop_mean_2 := (1-p)/p, .(p)]

## Our two pop means are not quite the same... why?
## Because we cut X of at 10, but in reality, X = [0, Inf]
for(i in c(10, 50, 100)) {
  p1 <- .2
  p2 <- .5
  p3 <- .8
  x <- 0:i
  y1 <- dgeom(x, p1)
  y2 <- dgeom(x, p2)
  y3 <- dgeom(x, p3)
  p <- rep(c(p1,p2,p3), each=length(x))
  d <- data.table(x,y1,y2,y3)
  dd <- melt(d, measure.vars = c('y1', 'y2', 'y3'))
  dd[, p := p]

  ## print(dd[, sum(x*value), .(p)])
  ## print(dd[, (1-p)/p, .(p)])
  print(dd[, sum(x*value) - (1-p)/p, .(p)])
}


## NOTE: Plot samples from distributions with different parameter values

## define the probability of success
p1 <- .2
p2 <- .5
p3 <- .8

## how many samples do we want to draw?
n <- 10000

## Sample from a geometric distribution
s1 <- rgeom(n, p1)
s2 <- rgeom(n, p2)
s3 <- rgeom(n, p3)

## Q: what are the sample means of these samples, and what are they an estimate of?
mean(s1)
mean(s2)
mean(s3)

## A: the sample mean is an estimate for the population mean
## Recall that for the geometric distribution, the expected value (also called
## the population mean) is given by (1-p) / p
(1-p1) / p1
(1-p2) / p2
(1-p3) / p3

## start assembling a data.table that we can use to plot an estimated PMF
d1 <- data.table(s1)
d2 <- data.table(s2)
d3 <- data.table(s3)

dd1 <- data.table(table(d1))
dd2 <- data.table(table(d2))
dd3 <- data.table(table(d3))

dd1[, p := p1]
dd2[, p := p2]
dd3[, p := p3]

dd <- rbindlist(list(dd1,dd2,dd3))

setnames(dd, 'd1', 'x')
dd[, p_est := N / n]
dd[, x := as.integer(x)]

ggplot(dd, aes(x=x, y=p_est)) +
  geom_bar(stat='identity') +
  theme(aspect.ratio = 1)  +
  facet_wrap(~p, scales='free')


## NOTE: explore the difference between sample mean and population mean here

## We need some trickery to get sample means here (because I coded this section
## sub-optimally), so we will repeat a lot of code.

## how many samples do we want to draw?
n <- 10000

## Sample from a geometric distribution
s1 <- rgeom(n, p1)
s2 <- rgeom(n, p2)
s3 <- rgeom(n, p3)

## start assembling a data.table that we can use to plot an estimated PMF
d1 <- data.table(s1)
d2 <- data.table(s2)
d3 <- data.table(s3)

## This time, when creating data.table from samples, include sample mean
dd1 <- data.table(table(d1), sample_mean=mean(s1))
dd2 <- data.table(table(d2), sample_mean=mean(s2))
dd3 <- data.table(table(d3), sample_mean=mean(s3))

dd1[, p := p1]
dd2[, p := p2]
dd3[, p := p3]

dd <- rbindlist(list(dd1,dd2,dd3))

setnames(dd, 'd1', 'x')
dd[, p_est := N / n]
dd[, x := as.integer(x)]

## add pop_mean
dd[, pop_mean := 1/p]

ggplot(dd, aes(x=x, y=p_est)) +
  geom_bar(stat='identity') +
  theme(aspect.ratio = 1)  +
  facet_wrap(~p, scales='free') +
  geom_vline(aes(xintercept=sample_mean, colour='red')) +
  geom_vline(aes(xintercept=pop_mean, colour='blue'))


## NOTE: run the experiment a few times to see the variation in sample mean, and
## the absence of variance in the population mean

## Again, for convenience, we'll just copy the relevant code chunks
num_exps <- 3
dd_rec  <- vector(mode='list', length=num_exps)

for(i in 1:num_exps) {

  ## define the probability of success
  p1 <- .2
  p2 <- .5
  p3 <- .8

  ## how many samples do we want to draw?
  ## TODO: This is an interesting parameter to play with
  n <- 100

  ## Sample from a geometric distribution
  s1 <- rgeom(n, p1)
  s2 <- rgeom(n, p2)
  s3 <- rgeom(n, p3)

  ## start assembling a data.table that we can use to plot an estimated PMF
  d1 <- data.table(s1)
  d2 <- data.table(s2)
  d3 <- data.table(s3)

  ## This time, when creating data.table from samples, include sample mean
  dd1 <- data.table(table(d1), sample_mean=mean(s1))
  dd2 <- data.table(table(d2), sample_mean=mean(s2))
  dd3 <- data.table(table(d3), sample_mean=mean(s3))

  dd1[, p := p1]
  dd2[, p := p2]
  dd3[, p := p3]

  dd <- rbindlist(list(dd1,dd2,dd3))

  setnames(dd, 'd1', 'x')
  dd[, p_est := N / n]
  dd[, x := as.integer(x)]

  ## add pop_mean
  dd[, pop_mean := 1/p]

  ## add indicator column to index experiment number
  dd[, exp := i]

  dd_rec[[i]] <- dd
}

dd <- rbindlist(dd_rec)

## TODO: this plot can be cleaned up... as can the entire programming structure
## of the section...
ggplot(dd, aes(x=x, y=p_est)) +
  geom_bar(stat='identity') +
  theme(aspect.ratio = 1)  +
  facet_wrap(~p*exp, scales='free') +
  geom_vline(aes(xintercept=sample_mean, colour='red')) +
  geom_vline(aes(xintercept=pop_mean, colour='blue'))


## NOTE: Joint distributions of two random variables
## A small factory operates on two shifts daily. In a study concerning the
## pattern of worker abseteeism, the random variables of interest are:

## X = number of absentees from the morning shift
## Y= number of absentees from evening shift of the same day

## Based on a long series of past attendence records, the personnel manager
## provides the assessment of the **joint** probability distribution of X and Y
## shown in the following table:

## |        x/y |   0 |   1 |   2 |   3 | Row Sum |
## |------------+-----+-----+-----+-----+---------|
## |          0 | .05 | .05 | .10 |   0 |      .2 |
## |          1 | .05 | .10 | .25 | .10 |      .5 |
## |          2 |   0 | .15 | .10 | .05 |     .30 |
## |------------+-----+-----+-----+-----+---------|
## | Column sum |  .1 | .30 | .45 | .15 |       1 |

## We will next ask and answer some questions about these random variables.
## Begin by building a data.table representation of the joint table
x_levels <- 0:2
y_levels <- 0:3

x <- rep(x_levels, each=4)
y <- rep(y_levels, 3)
p <- c(.05, .05, .1, .0, .05, .10, .25, .10, 0, .15, .10, .05)
d <- data.table(x, y, p)

## What is the probability of 2 absences in the morning?
## That is, what is P(X=2)
## We see that X=2 for a variety of Y values:
d[x==2]

## To get P(X=2), just add the various ways up
d[x==2, sum(p)]

## What is P(X > Y)?
d[x > y, sum(p)]

## What is E(X) and E(Y)
mu_x <- d[, sum(x*p)]
mu_y <- d[, sum(y*p)]

## What is Var(X) and Var(Y)?
var_x <- d[, sum((x^2)*p) - mu_x^2]
var_y <- d[, sum((y^2)*p) - mu_y^2]

## What is the expected number of total absences each day?
## In general: E(Z = X + Y) = E(X) + E(y)
mu_xy <- mu_x + mu_y

## We can also do this with the original simple formula
## E(Z) = sum(z*p_z) = sum((x + y)*p_xy)
mu_xy <- d[, sum((x+y)*p)]


## NOTE: Covariance and Correlation
## Covariance is a numerical measure of the joint variation of two random
## variables
## Cov(X,Y) = E((X-mu_x)*(Y-mu_y))
## Cov(X,Y) = E(XY) - mu_x*mu_y
cov_xy <- d[, sum((x*y)*p) - mu_x*mu_y]

## The correlation coefficient is often used in place of covariance
## Corr(X,Y) = Cov(X,Y) / (sd_X * sd_Y)
corr_xy <- cov_xy / (sqrt(var_x) * sqrt(var_y))


## NOTE: Independence of two random variables
## Random variables X and Y are independent if P(X,Y) = P(X)*P(Y) for all
## possible pairs of (X,Y) in the joint distribution.

## Independence implies zero covariance and zero correlation
## Zero correlation does not imply independence
