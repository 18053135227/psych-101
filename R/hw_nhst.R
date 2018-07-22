library('data.table')
library('ggplot2')
library('gtools')

rm(list = ls())

## NOTE: What is a random variable?


## NOTE: Describe the following following distributions and give examples of
## random variables that might be well described by them. be sure to specifiy
## the parameters that uniquely defines each distribution and describe what
## aspects of the distribution they influence.

## (a) Bernoulli
## (b) Binomial
## (c) Normal
## (d) t


## NOTE: What is the sampling distribution?


## NOTE: What is the central limit theorem?


## NOTE: Verify the central limit theorem using:
## X ~ F(d1, d2) for d1=5, d2=100
## For these values of d1 and d2,
## E[X] = d2 / (d2 - 2)
## Var[X] = (2 * d2^2 * (d1 + d2 -2)) / (d1 * (d2 - 2)^2 * (d2 - 4))

## (a) Compute the population mean and variance for X

## (b) Perform 6 experiments. In each experiment, draw 1000 samples from X. Plot
## a histogram for the results of each experiment (6 panels on one figure). Do
## the histograms appear Normally distributed? If not, what is the main
## difference?

## (c) Estimate the distribution of sample means as a function of the number of
## samples we drawn per experiment. Perform 6 experiments in which you draw 2,
## 10, 20, 30, 50, and 100 samples from X (each of these numbers corresponds to
## one experiment). Each of these experiments, if performed once, will give you
## one sample mean. To examine the distribution of sample means, repeat each
## experiment 1000 times. This will give you 1000 sample means per experiment,
## which is plenty for a good histogra. Plot your results in a figure containing
## one panel per experiment. For each experiment, estimate the distribution of
## sample means with a histogram.

## (d) At what value of n do you think the distribution of sample means looks
## like a Normal distribution?

## (e) What happens to the variance of the distribution of sample means as n
## increases? Verify what you see in the plot above by print it out via
## data.table.

## (f) What is the population mean and population variance of the distribution
## of sample means (hint: what does the central limit theorem say it shoud be)?
## Do your data agree?

## (g) How awesome do you think the Central Limit Theorem is?


## NOTE: Analyze real data

## (a) Load the data "ii_gabor.csv" into a data.table

## (b) Change the column names to the following
col_names <- c(
  'cat',
  'x',
  'y',
  'resp',
  'rt',
  'phase'
)

## (c) Add a column to the data.table that indicates the trial of the experiment

## (d): Add a column to the data.table that indicates the block of the
## experiment, assuming a block size of 100 trials per block.

## (e) Add a column to the data.table that indicates whether the response on
## each trial was correct or incorreect (e.g., cat == resp)

## (d) Create a new data.table (I'll call it "dd") that contains columns for the
## mean accuracy per block and the SEM per block, grouped by each unique
## combination of block and phase.

## (f) Add a column to the data.table that indicates whether the phase of the
## experiment has changed (i.e., is different in the current row than it was in
## the previous row).

## (h) Plot the mean accuracy per block (x=block, y=acc_mean_per_block). Include
## error bars at each data point that represent SEM. plot phase 1 and phase 2 in
## different colors.


## NOTE: Real data NHST --- the Binomial approach
## Each trial is a Bernouli trial with probability of success p.
## This means that trials are distributed X ~ Bernoulli(p).
## This also means that blocks of trials are distributed X ~ Binomial(n,p),
## where n = block_size.

## (a) Test the hypothesis that, for the first 100 trials of phase 1, the monkey
## is doing better than guessing (i.e., p = .5). Do this with and without
## binom.test() and make sure that your answers agree with each other. Also,
## plot the Null distribution and use vertical lines to indicate critical values
## and observed values.

## (b) Test the hypothesis that, for the last 100 trials of phase 1, the monkey
## is doing better than guessing (i.e., p = .5). Do this with and without
## binom.test() and make sure that your answers agree with each other. Also,
## plot the Null distribution and use vertical lines to indicate critical values
## and observed values.

## (c) Test the hypothesis that, for the first 100 trials of phase 2, the monkey
## is doing worse than guessing (i.e., p = .5). Do this with and without
## binom.test() and make sure that your answers agree with each other. Also,
## plot the Null distribution and use vertical lines to indicate critical values
## and observed values.


## NOTE: What accounts for the drop in accuracy between phase 1 and phase 2? To
## answer this question, investigate the stimuli the monkey was required to
## learn between phase 1 and phase 2. Make a plot of stimuli (x=x, y=y) coloured
## by category membership label. Inlcude a seperate version of this plot for
## each phase in different "facets".


## NOTE:
## Above, we viewed each trial as Bernoulli(p), and each n trials as
## Binomial(n,p). An alternative framing is to ignore each individual trial, and
## instead consider the random variable,

## X = mean accuracy per block

## In this framing, the experiment results are sampled from blocks of trials,
## not from indivisual trials.


## NOTE: Prep phase for next bits

## (a) Redefine the block column of your original data.table "d" to use 25
## trials per block instead of 100.

## (b) Redefine "dd" to incorporate the new block size

## (c) Repeat the mean accuray plot using the new block size. Be warned that
## this plot will look pretty "busy"... too busy for a paper or a report, but
## fine for our purposes below.


## NOTE: NHST using X = mean accuracy per block

## (a) Is the mean accuracy in the first 16 blocks of phase 1 greater than
## chance? Do this with and without t.test() and make sure that your answers
## agree with each other. Also, plot the Null distribution and use vertical
## lines to indicate critical values and observed values.

## (b) Is the mean accuracy in the last 16 blocks of phase 1 greater than
## chance? Do this with and without t.test() and make sure that your answers
## agree with each other. Also, plot the Null distribution and use vertical
## lines to indicate critical values and observed values.

## (c) Is the mean accuracy in the first 16 blocks of phase 2 less than chance?
## Do this with and without t.test() and make sure that your answers agree with
## each other. Also, plot the Null distribution and use vertical lines to
## indicate critical values and observed values.

## (d) Assume that the true state of the universe is mu = .6 for the final 16
## blocks of phase 1. Compute alpha, beta, and power for H0: mu = .5.

## (e) Plot the Null and Alternative distributions. Indicate the critical value,
## observed value, and color the areas corresponding to alpha, beta, and power.

## (f) Plot power as a function of n
