library('data.table')
library('ggplot2')

## Goal: Use NHST to test if a coin is fair

## 1. State the Hypotheses
## H0: P(heads) = P(tails)
## H1: P(heads) != P(tails)

## 2. Determine what you will measure
## We will do the obvious thing: Flip the coin a bunch of times and count up the
## number of heads and tails that result

## 3. Determine how confident you need to be and what sorts of errors are acceptable
## We hate cheaters, so our only concern is to find them.
## This means that we are pretty eager to reject H0.
## So maybe we don't need the evidence to be very strong before we act.

## 4. Determine a decision rule
## Hmm. If we flip the coin 100 times, how many more or less heads than tails
## will we need to conclude that it's a cheater coin?
## Below, we will run some simulations to try to get a feel for what might be a
## good policy here.

## TODO:
## Suppose we can tolerate alpha = 0.05
## That is, we can false alarm 1 out of every 20
## how many more heads then tails or vice versa then?
## We want to end up asking:
## If H0 is true, what is the probability of observing our data?
## If that probability is high, then we fail to reject, etc.
## What is the probability of 5 heads in 10 flips?
## https://math.stackexchange.com/questions/151810/probability-of-3-heads-in-10-coin-flips

## 5. Make a decision
## This would be easy if we had a good decision rule (step 4).

## Define the probability of heads
p = 0.5

## Flip the coin (esxecute this line a bunch of times to see the variability)
rbinom(1,1,p)

## Flip the coin a few times and save the results
n_exps <- 5
n_flips <- 100
n_rows <- n_exps * n_flips

# Define some empty vectors to hold onto experiment results
exp_record = c()
flip_record = c()
outcome_record = c()

## Flip the coin a bunch of times
## We will flip it n_flips times in each of n_exps experiments
for(i in 1:n_exps) {
  for(j in 1:n_flips) {
    exp_record <- c(exp_record, i)
    flip_record <- c(flip_record, i)
    outcome_record <- c(outcome_record, rbinom(1,1,p))
  }
}

## Convert to data.table for awesomeness
d <- data.table('exp'=exp_record, 'flip'=flip_record, 'outcome'=outcome_record)

## Convert 0 and 1 to a factor with labels 'heads' 'tails'
d[, outcome := factor(outcome, levels = c(0,1), labels = c('heads', 'tails'))]

## Plot the results of our experiment
ggplot(d, aes(outcome)) +
  geom_bar(stat='count') +
  facet_wrap(~exp, ncol=5)
