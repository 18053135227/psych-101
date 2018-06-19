library(data.table)
library(ggplot2)

rm(list=ls())

## NOTE: 1
## The following measurements of weight (in grams) have been recorded for a
## common strain of 70 31-day-old rats.

rat_weights <- c(120, 116, 94, 120, 112, 112, 106, 102, 118, 112, 116, 98, 116,
                 114, 120, 124, 112, 122, 110, 84, 106, 122, 124, 112, 118, 128,
                 108, 120, 110, 106, 106, 102, 140, 102, 122, 112, 110, 130,
                 112, 114, 108, 110, 116, 118, 118, 108, 102, 110, 104, 112,
                 122, 112, 116, 110, 112, 118, 98, 104, 120, 106, 108, 110, 102,
                 110, 120, 126, 114, 98, 116, 100)

d <- data.table(rat_weights)

## (a) Plot a relative frequency histogram of these data using bin size = 5
b <- seq(d[, min(rat_weights)], d[, max(rat_weights)], 5)
ggplot(d, aes(x=rat_weights)) +
  geom_histogram(
    aes(y=..density..),
    breaks=b
  ) +
  geom_density() +
  theme(aspect.ratio = 1)

## (b) Plot a relative frequency histogram of these data using bin size = 2
b <- seq(d[, min(rat_weights)], d[, max(rat_weights)], 2)
ggplot(d, aes(x=rat_weights)) +
  geom_histogram(
    aes(y=..density..),
    breaks=b
  ) +
  geom_density() +
  theme(aspect.ratio = 1)

## (c) Add mean and sd indicators to the histogram
dd <- d[, c('value_mean', 'value_sd') := .(mean(rat_weights), sd(rat_weights))]
b <- seq(d[, min(rat_weights)], d[, max(rat_weights)], 2)
ggplot(dd, aes(x=rat_weights)) +
  geom_histogram(
    aes(y=..density..),
    breaks=b
  ) +
  geom_density() +
  theme(aspect.ratio = 1) +
  geom_vline(aes(xintercept = value_mean, colour='red')) +
  geom_segment(aes(
    x=value_mean,
    xend=value_mean+value_sd,
    y=0.1,
    yend=0.1,
    colour='red')) +
  geom_segment(aes(
    x=value_mean,
    xend=value_mean-value_sd,
    y=0.1,
    yend=0.1,
    colour='red'))


## NOTE: 2
## In a genetic study, a regular food was placed in each of 20 vials and the
## number of flies of a particular genotype feeding on each vialwas recorded.
## The counts of the flies were also recorded for another set of 20 vials that
## contained grape juice. The following data sets were obtained:

n_flies_regular_food <- c(15, 20, 31, 16, 22, 22, 23, 33, 38, 28, 25, 20, 21,
                          23, 29, 26, 40, 20, 19, 31)

n_flies_grape_juice <- c(6, 19, 0, 2, 11, 12, 13, 12, 5, 16, 2, 7, 13, 20, 18,
                         19, 19, 9, 9, 9)

## (a) Plot seperate dot plots for the two data sets
d <- data.table(n_flies_regular_food, n_flies_grape_juice)
dd <- melt(d)

ggplot(dd, aes(x=value)) +
  geom_dotplot() +
  facet_wrap(~variable) +
  theme(aspect.ratio = 1)

## (b) Make a visual comparison of the two distributions with respect to their
## relative locations and spreads. To do this, plot a vertical line at the mean
## value of each distribution on top of each dot plot, and a horizontal line
## centered at the mean value and extending 1 standard deviation in either
## direction.

dd[, c('value_mean', 'value_sd') := .(mean(value), sd(value)), .(variable)]
ggplot(dd, aes(x=value)) +
  geom_dotplot() +
  facet_wrap(~variable) +
  theme(aspect.ratio = 1) +
  geom_vline(aes(xintercept = value_mean, colour='red')) +
  geom_segment(aes(
    x=value_mean,
    xend=value_mean+value_sd,
    y=0.5,
    yend=0.5,
    colour='red')) +
  geom_segment(aes(
    x=value_mean,
    xend=value_mean-value_sd,
    y=0.5,
    yend=0.5,
    colour='red'))


## (c) Plot each distribution as a relative frequency historgam. Overlay a
## smooth curve estimate of the density. Plot both distributions in the same
## panel. Add mean and sd indicators to the plot.
ggplot(dd, aes(x=value, fill=variable, alpha=.25)) +
  geom_histogram(aes(y=..density..)) +
  geom_density() +
  theme(aspect.ratio = 1) +
  geom_vline(aes(xintercept = value_mean, colour=variable)) +
  geom_segment(aes(
    x=value_mean,
    xend=value_mean+value_sd,
    y=0.3,
    yend=0.3,
    colour=variable)) +
  geom_segment(aes(
    x=value_mean,
    xend=value_mean-value_sd,
    y=0.3,
    yend=0.3,
    colour=variable))

## (d) Do you think the flies like one food more than the other? Which food
## would you bet they liked more? How confident are you in your guess? Why?


## NOTE: 3
## (a) load the built in data set "trees" into a data.table
d <- as.data.table(trees)

## (b) plot a separate relative frequency histogram for "Girth", "height",
## "Volume". For each, overlay a smooth curve estimate of the denstity,
## andindicate mean values via vertical lines.
dd <- melt(d)
dd[, c('value_mean', 'value_sd') := .(mean(value), sd(value)), .(variable)]
ggplot(dd, aes(x=value, fill=variable, alpha=.25)) +
  geom_histogram(aes(y=..density..)) +
  geom_density(aes(fill=variable)) +
  theme(aspect.ratio = 1) +
  geom_vline(aes(xintercept=value_mean, colour=variable))

## (c) Suppose that in a bet, you are required to place a bet on the numerical
## value of one of "Girth", "height", "Volume". Which would you choose to bet
## on, and what numerical value would you bet on?
