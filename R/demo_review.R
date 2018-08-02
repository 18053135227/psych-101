library('data.table')
library('ggplot2')

################################################################################
################################################################################

## NOTE: Review and consolidation of NHST

## First, perform an experiment / gather some data.
## Then let,
## X = The RV that generates data for your experiment
## x = a sample from the RV X

## Step 1: state hypotheses

## Step 2: choose confidence

## Step 3: specify the distribution of a statistic that estimates the population
##         parameter in step 1 and compute it's value from your x observations
##         (for now, call the result x_H_obs).
## X ~ how do you think X is distributed?
## X_H_obs = what statistic will you use to estimate the parameters in H0?
## X_H_obs ~ how do you think X_H_obs is distributed?

## Step 4: Determine rejection criteria.
##         - compute a p-value: P(x_H_obs | H0 is true)
##         - compute critical values: x_H_crit = the most extreme value of
##         - x_H_obs that we will tolerate before we would decide to reject H0.

## Step 5: Make a decision.

################################################################################
################################################################################

## NOTE: If the class is a representative sample of all students at UC Berkeley,
## then test the hypothesis that the probability of a UCB student is wearing
## shoes with laces is greater than chance.

x <- c(F, F, T, T, F, F, T, F, F, T, F, T, T, F, T, T, F, F, T)

## (a) what type of test will we do?
## Binomial test

## (b) do the test long hand / code
## step 1:
## H0: p = 0.5
## H1: p > 0.5
pH0 <- 0.5

## step 2:
alpha <- 0.05

## step 3:
## X ~ Bernoulli(p)
## X_H_obs = number of successes
## X_H_obs ~ Binomial(n, p)
num_successes <- sum(x)
n <- length(x)

## step 4:
p_obs <- pbinom(num_successes-1, n, pH0, lower.tail=FALSE)
x_crit <- qbinom(alpha, n, pH0, lower.tail=FALSE)

## step 5:
if(p_obs < alpha) {
  print('Reject H0')
} else {
  print('Fail to reject H0')
}

if(num_successes > x_crit) {
  print('Reject H0')
} else {
  print('Fail to reject H0')
}

## (c) do the test short hand / code
## binom.test(x, n, p = 0.5,
##            alternative = c("two.sided", "less", "greater"),
##            conf.level = 0.95)
binom.test(num_successes, n, pH0, alternative='greater', conf.level=1-alpha)


## NOTE: If the class is a representative sample of all students at UC Berkeley,
## then test the hypothesis that the average height of all students at UC
## Berkeley is greater than 5'5".

x <- c(5.8, 5.7, 6.0, 6.3, 5.7, 5.4, 5.8, 5.10, 5.9, 5.5, 5.3, 6.0, 5.8, 5.4,
5.7, 5.2, 5.8, 10.0, 5.6, 4.3, 3.5)

## (a) what type of test will we do?
## t-test

## (b) do the test long hand / code
## step 1:
## H0: mu = 5.5
## H1: mu > 5.5
muH0 <- 5.5

## step 2:
alpha <- 0.05

## step 3:
## X ~ Normal(mu, sigma)
## X_H_obs = sample mean of x
## X_H_obs ~ Normal(mu, sigma/sqrt(n))
x_H_obs <- mean(x)
n <- length(x)
sigma_est <- sd(x) / sqrt(n)
t_obs <- (x_H_obs - muH0) / sigma_est

## step 4:
df <- n-1
p_obs <- pt(t_obs, df, lower.tail=FALSE)
t_crit <- qt(alpha, df, lower.tail=FALSE)

## step 5:
if(p_obs < alpha) {
  print('Reject H0')
} else {
  print('Fail to reject H0')
}

if(t_obs > t_crit) {
  print('Reject H0')
} else {
  print('Fail to reject H0')
}

## (c) do the test short hand / code
## t.test(x, y = NULL,
##        alternative = c("two.sided", "less", "greater"),
##        mu = 0, paired = FALSE, var.equal = FALSE,
##        conf.level = 0.95, ...)
t.test(x, mu=muH0, alternative = 'greater', conf.level = 1 - alpha)


## NOTE: If the class is a representative sample of all students at UC Berkeley,
## then test the hypothesis that the proabbility of left sitting is greater than
## .5

x <- c(rep(TRUE, 11), rep(FALSE, 9))

## (a) what type of test will we do?
## Binomial test

## (b) do the test long hand / code
## (c) do the test short hand / code
binom.test(sum(x), length(x), .5, alternative='greater', .95)


## NOTE: If the class is a representative sample of all students at UC Berkeley,
## then test the hypothesis that the number of units a student is enrolled in
## this summer is less then the number of units they were enrolled in during the
## previous academic session.

## current
x <- c(7, 5, 3, 4, 7, 7, 3, 8, 7, 12, 6, 8, 8, 10, 8, 10, 8, 10)

## previous
y <- c(15, 18, 13, 13, 8, 17, 15, 15, 16, 17, 19, 19, 19, 18, 13, 10, 21, 15)

nx <- length(x)
ny <- length(y)

## (a) what type of test will we do?
## paired samples t-test
## current - previous
d <- x - y

## (b) do the test long hand / code
## (c) do the test short hand / code
## t.test(x, y = NULL,
##        alternative = c("two.sided", "less", "greater"),
##        mu = 0, paired = FALSE, var.equal = FALSE,
##        conf.level = 0.95, ...)
t.test(x, y, alternative = 'less', paired=TRUE)
t.test(d, alternative = 'less')

## NOTE: If the class is a representative sample of all students at UC Berkeley,
## then test the hypothesis that the variance in height between all students at
## UC Berkeley is greater than 10

x <- c(5.8, 5.7, 6.0, 6.3, 5.7, 5.4, 5.8, 5.10, 5.9, 5.5, 5.3, 6.0, 5.8, 5.4,
       5.7, 5.2, 5.8, 10.0, 5.6, 4.3, 3.5)

## (a) what type of test will we do?
## chisq test

## (b) do the test long hand / code
## step 1:
## H0: sigmasq = 10
## H1: sigmasq > 10
sigmasqH0 <- 10

## step 2:
alpha <- 0.05

## step 3:
## X ~ N(mu, sigma)
## X_H_obs = sample variance of x
## X_H_obs ~ chisq(df), df = n-1 (more or less: LOOK AT YOUR NOTES!)
df <- n - 1
chisq_obs = df * var(x) / sigmasqH0
x_H_obs <- chisq_obs

## step 4:
p_obs <- pchisq(x_H_obs, df, lower.tail=FALSE)
chisq_crit <- qchisq(alpha, df, lower.tail=FALSE)

## step 5:
if(p_obs < alpha) {
  print('Reject H0')
} else {
  print('Fail to reject H0')
}

if(x_H_obs > chisq_crit) {
  print('Reject H0')
} else {
  print('Fail to reject H0')
}

## (c) do the test short hand / code
library('EnvStats')
varTest(x, alternative='greater', sigma.squared=10)


## NOTE: If the class is a representative sample of all students at UC Berkeley,
## then test the hypothesis that the variance in height is greater in students
## that sit on the left side of class versus students that sit on the right side
## of class.

## (a) what type of test will we do?
## (b) do the test long hand / code
## (c) do the test short hand / code


## NOTE: ANOVA...
