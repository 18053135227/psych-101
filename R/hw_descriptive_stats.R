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


## (a) Plot a relative frequency histogram of these data using bin size = 5

## (b) Plot a relative frequency histogram of these data using bin size = 2

## (c) Add mean and sd indicators to the histogram


## NOTE: 2
## In a genetic study, a regular food was placed in each of 20 vials and the
## number of flies of a particular genotype feeding on each vialwas recorded.
## The counts of the flies were also recorded for another set of 20 vials that
## contained grape juice. The following data sets were obtained:


## (a) Plot seperate dot plots for the two data sets

## (b) Make a visual comparison of the two distributions with respect to their
## relative locations and spreads. To do this, plot a vertical line at the mean
## value of each distribution on top of each dot plot, and a horizontal line
## centered at the mean value and extending 1 standard deviation in either
## direction.


## (c) Plot each distribution as a relative frequency historgam. Overlay a
## smooth curve estimate of the density. Plot both distributions in the same
## panel. Add mean and sd indicators to the plot.

## (d) Do you think the flies like one food more than the other? Which food
## would you bet they liked more? How confident are you in your guess? Why?


## NOTE: 3
## (a) load the built in data set "trees" into a data.table

## (b) plot a separate relative frequency histogram for "Girth", "height",
## "Volume". For each, overlay a smooth curve estimate of the denstity,
## andindicate mean values via vertical lines.

## (c) Suppose that in a bet, you are required to place a bet on the numerical
## value of one of "Girth", "height", "Volume". Which would you choose to bet
## on, and what numerical value would you bet on?
