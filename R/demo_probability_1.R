library('data.table')
library('ggplot2')

## Goal: Uncerstand some basic concepts of probability through the magic of R

## Consider the experiment of tossing a balanced die and recording the number on
## the face that shows up. Each of the 6 faces on this symmetric cube is as
## likely to appear as any other. Without performing the experiment, we can
## logically conclude that the proportion of times each particular face is
## expected to occur is 1/6. Any other assignment of probability would not treat
## each face equally.

## define the sample space (possible outcomes) of a die roll
die_1 <- c(1,2,3,4,5,6)

## define the probability of each outcome (here, equally likely)
## Note that these are weights used in the sample() function below.
## They are not true probabilities.
## You can tell they're not true probabilities because they don't sum up to 1.
p_die_1 <- rep(1, 6)

## roll the die
n_rolls <- 1
sample(die_1, n_rolls, replace=TRUE, prob=p_die_1)

## We have defined a balanced die (all possible outcomes are equally likely).
## Does that mean that, if we roll the die a bunch of times, we will always get
## equal proportions of each outcome?
## The answer is definitely no.

## Perform 5 experiments, and roll the die 50 times in each.
n_exps <- 5
n_rolls <- 50
exp_number <- c()
exp_results <- c()
for(i in 1:n_exps) {
  exp_number <- c(exp_number, rep(i, n_rolls))
  exp_results <- c(exp_results, sample(die_1, n_rolls, replace=TRUE, prob=p_die_1))
}

## convert results from vectors to data.table
d <- data.table('exp_number'=exp_number, 'exp_results'=exp_results)

## plot the results
ggplot(d, aes(x=exp_results)) +
  geom_bar(aes(y=(..count..)/sum(..count..))) +
  facet_wrap(~exp_number, ncol=5)

## The point is to see that experiment outcomes are variable.
## Even when we the underlying probability of each possible event is equal
## If you didn't know I programmed them this way, and all I gave you was the
## figure at the end, could you guess whether it was a balanced die?

## How about if I told you that it was unbalanced, and then offered you a
## million dollars to guess the probability of each outcome? A rational approach
## is still to roll the die a bunch of times, and record the proportion of times
## each face comes up. But what about the VARIABILITY, you say? Turns out we can
## vastly reduce the effect of variability by rolling the die LOTS of times.

## First, lets repeat the above experiment, but with many more rolls per experiment
## Perform 5 experiments, and roll the die 1000 times in each.
n_exps <- 5
n_rolls <- 10000
exp_number <- c()
exp_results <- c()
for(i in 1:n_exps) {
  exp_number <- c(exp_number, rep(i, n_rolls))
  exp_results <- c(exp_results, sample(die_1, n_rolls, replace=TRUE, prob=p_die_1))
}

## convert results from vectors to data.table
d <- data.table('exp_number'=exp_number, 'exp_results'=exp_results)

## plot the results
ggplot(d, aes(x=exp_results)) +
  geom_bar(aes(y=(..count..)/sum(..count..))) +
  facet_wrap(~exp_number, ncol=5)

## Now, lets try it with a lop sided die
die_2 <- c(1,2,3,4,5,6)
p1 = .75 / 5
p2 = .25
5*p1 + p2
p_die_2 <- c(p1,p1,p1,p1,p1,p2)

n_exps <- 5
n_rolls <- 10000 ## You can play with this to see that it's not so obvious for small n_rolls
exp_number <- c()
exp_results <- c()
for(i in 1:n_exps) {
  exp_number <- c(exp_number, rep(i, n_rolls))
  exp_results <- c(exp_results, sample(die_2, n_rolls, replace=TRUE, prob=p_die_2))
}

d <- data.table('exp_number'=exp_number, 'exp_results'=exp_results)

ggplot(d, aes(x=exp_results)) +
  geom_bar(aes(y=(..count..)/sum(..count..))) +
  facet_wrap(~exp_number, ncol=5)

## An estimate of the probability of each event is the proportion of times that
## event occurs in a sample (result of an experiment)
d[, p_est := .N/n_rolls, .(exp_number, exp_results)]

## we know the 6th side is the heavy side, so lets focus on that
## recall that for a fair die, P(1)=P(2)=...=P(6) = 1/6 = 0.167
d[exp_results==6, unique(p_est), .(exp_number)]

## Lets investigate more formally how sample size effects our estimate of P(6)
n_rolls <- seq(10,500,5)
n_exps <- length(n_rolls)
exp_number <- c()
exp_results <- c()
for(i in 1:n_exps) {
  exp_number <- c(exp_number, rep(i, n_rolls[i]))
  exp_results <- c(exp_results, sample(die_2, n_rolls[i], replace=TRUE, prob=p_die_2))
}

d <- data.table('exp_number'=exp_number, 'exp_results'=exp_results)
d[, n_rolls := .N, .(exp_number)]
d[, p_est := .N/n_rolls, .(exp_number, exp_results)]

ggplot(d[exp_results==6], aes(x=n_rolls, y=p_est)) +
  geom_point() +
  geom_hline(yintercept=p2, colour='red') +
  ylim(0,.5)
