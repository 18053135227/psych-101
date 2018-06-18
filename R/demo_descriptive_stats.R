library(data.table)
library(ggplot2)

rm(list=ls())

## NOTE: Example 1
## To study the possible effects of a new noise pollution ordinance, 18 power
## lawn mowers were observed and their noise levels were recorded to the nearest
## decibel. The following data were obtained.

noise_levels <- c(95,120,117,99,110,107,125,98,85,127,105,114,103,112,92,101,122,120)
mower_id <- 1:length(noise_levels)

d <- data.table(mower_id, noise_levels)

## Show noise levels as a dot plot
ggplot(d, aes(x=noise_levels)) +
  geom_dotplot() +
  theme(aspect.ratio = 1)

## add mean and median to the plot
## Here, there are no outliers, so mean and median are virtually identical
noise_mean <- d[, mean(noise_levels)]
noise_median <- d[, median(noise_levels)]
ggplot(d, aes(x=noise_levels)) +
  geom_dotplot() +
  theme(aspect.ratio = 1) +
  geom_vline(xintercept = noise_mean, colour = 'red') +
  geom_vline(xintercept = noise_median, colour = 'blue')


## NOTE: Example 2
## Waiting to cross a busy street on the way to class one morning, Professor J.
## noted the following times in seconds between cars traveling in the same
## direction:

times <- c(6,3,5,6,4,3,5,4,6,3,4,5,4,18)
times_id <- 1:length(times)

d <- data.table(times_id, times)

ggplot(d, aes(x=times)) +
  geom_dotplot() +
  theme(aspect.ratio = 1)

## The dotplot reveals that the times are closely grouped around 4 or 5, except
## for one outlier. Prof J. noticed this outlier, investigated what was up with
## it, and discovered that there was a stop light a block away that had turned
## red. The entire point of this silly story is that we will always want to
## identify outliers in our data and investigate them.

## add mean and median to the plot
## Here, you can see that the mean, but not the median, is sensitive to outliers.
noise_mean <- d[, mean(times)]
noise_median <- d[, median(times)]
ggplot(d, aes(x=times)) +
  geom_dotplot() +
  theme(aspect.ratio = 1) +
  geom_vline(xintercept = noise_mean, colour = 'red') +
  geom_vline(xintercept = noise_median, colour = 'blue')


## NOTE: Example 3
## One of the major indicators of air pollution in large cities and industrial
## belts is the concentration of ozone in the atmosphere. From massive data
## collected by LA County authorities, 78 measurements of ozone concentration in
## the downtown LA area during the summer of 1966 and 1967 are recorded in the
## table below. Each measurement is an average of hourly readings taken every
## fourth day.

ozone <- c(
  3.5, 1.4, 6.6, 6.0, 4.2, 4.4, 5.3, 5.6,
  6.8, 2.5, 5.4, 4.4, 5.4, 4.7, 3.5, 4.0,
  2.4, 3.0, 5.6, 4.7, 6.5, 3.0, 4.1, 3.4,
  6.8, 1.7, 5.3, 4.7, 7.4, 6.0, 6.7, 11.7,
  5.5, 1.1, 5.1, 5.6, 5.5, 1.4, 3.9, 6.6,
  6.2, 7.5, 6.2, 6.0, 5.8, 2.8, 6.1, 4.1,
  5.7, 5.8, 3.1, 5.8, 1.6, 2.5, 8.1, 6.6,
  6.8, 3.1, 4.7, 3.8, 5.9, 3.3, 6.2, 7.6,
  6.6, 4.4, 5.7, 4.5, 3.7, 9.4
  )

ozone_id <- 1:length(ozone)

d <- data.table(ozone, ozone_id)

## TODO: It would really be nice to combine plots into one figure

## Before prepping for this class, I'd never seen a dotplot. In my experience, a
## histogram is by far the more common diagnostic tool.
ggplot(d, aes(x=ozone)) +
  geom_histogram(
    col='black',
    fill='white') +
  theme(aspect.ratio = 1)

## You can control how big or small the bins are using the "breaks" argument
## small bins
## big bins
b <- seq(0,12,by=2.0)
ggplot(d, aes(x=ozone)) +
  geom_histogram(
    breaks=b,
    col='black',
    fill='white') +
  theme(aspect.ratio = 1)

b <- seq(0,12,by=0.25)
ggplot(d, aes(x=ozone)) +
  geom_histogram(
    breaks=b,
    col='black',
    fill='white') +
  theme(aspect.ratio = 1)

## geom_density() is often a nice addition to hsitograms
b <- seq(0,12,by=0.25)
ggplot(d, aes(x=ozone)) +
  geom_histogram(
    breaks=b,
    col='black',
    fill='white') +
  geom_density(colour='red') +
  theme(aspect.ratio = 1)

## If geom_density() is so cool, then why does the last figure look so crappy?
## "Density" normalizes the total area to 1 Regular old histogram doesn't do
## that... but we can tell it to:
b <- seq(0,12,by=0.25)
ggplot(d, aes(x=ozone)) +
  geom_histogram(
    aes(y=..density..),
    breaks=b,
    col='black',
    fill='white') +
  geom_density(colour='red') +
  theme(aspect.ratio = 1)

## add mean and median to the histogram
## small bins
b <- seq(0,12,by=0.25)
ggplot(d, aes(x=ozone)) +
  geom_histogram(
    aes(y=..density..),
    breaks=b,
    col='black',
    fill='white') +
  geom_density(colour='black', size=1.25) +
  theme(aspect.ratio = 1) +
  geom_vline(xintercept = d[, mean(ozone)], colour='red') +
  geom_vline(xintercept = d[, median(ozone)], colour='blue')

## slightly larger bins
b <- seq(0,12,by=1.0)
ggplot(d, aes(x=ozone)) +
  geom_histogram(
    aes(y=..density..),
    breaks=b,
    col='black',
    fill='white') +
  geom_density(colour='black', size=1.25) +
  theme(aspect.ratio = 1) +
  geom_vline(xintercept = d[, mean(ozone)], colour='red') +
  geom_vline(xintercept = d[, median(ozone)], colour='blue')

## TODO: pick up here
## This seems like a fine time to begin examing "spread"
## Add sample variance, standard deviation, and range to the plot
b <- seq(0,12,by=1.0)
ggplot(d, aes(x=ozone)) +
  geom_histogram(
    aes(y=..density..),
    breaks=b,
    col='black',
    fill='white') +
  geom_density(colour='black', size=1.25) +
  theme(aspect.ratio = 1) +
  geom_vline(xintercept = d[, mean(ozone)], colour='red') +
  geom_vline(xintercept = d[, median(ozone)], colour='blue') +
  geom_segment()

## # below, score_hw_x, is a place holder for your score on homework x
## scores_hw <- c(
##   score_hw_1,
##   score_hw_2,
##   score_hw_3,
##   score_hw_4,
##   score_hw_5,
##   score_hw_6,
##   score_hw_7,
##   score_hw_8
## )

## cumulative_score_hw <- sum(hw_scores)
## score_project <- x # x is a placeholder for your score

## mean(c(scores_hw, score_project))


## NOTE: Example 4
## The UCBAdmissions comes built in with R.
## https://www.rdocumentation.org/packages/datasets/versions/3.5.0
## This data set is frequently used for illustrating Simpson's paradox, see
## Bickel et al (1975). At issue is whether the data show evidence of sex bias
## in admission practices. There were 2691 male applicants, of whom 1198 (44.5%)
## were admitted, compared with 1835 female applicants of whom 557 (30.4%) were
## admitted. This gives a sample odds ratio of 1.83, indicating that males were
## almost twice as likely to be admitted. In fact, graphical methods (as in the
## example below) or log-linear modelling show that the apparent association
## between admission and sex stems from differences in the tendency of males and
## females to apply to the individual departments (females used to apply more to
## departments with higher rejection rates).
d <- as.data.table(UCBAdmissions)

## Play with a bunch of different ways to inspect if there is a gender
## difference

ggplot(d, aes(x=Gender, y=N)) +
  geom_bar(stat='identity') +
  facet_wrap(~Admit*Dept, ncol=6) +
  theme(aspect.ratio=1)

dd <- d[, sum(N), .(Gender, Admit)]
ggplot(dd, aes(x=Gender, y=V1)) +
  geom_bar(stat='identity') +
  facet_wrap(~Admit) +
  theme(aspect.ratio=1)

dd <- d[, N[Admit=='Admitted']/N[Admit=='Rejected'], .(Gender, Dept)]
ggplot(dd, aes(x=Gender, y=V1)) +
  geom_bar(stat='identity') +
  facet_wrap(~Dept, ncol=6) +
  theme(aspect.ratio=1)

## TODO:

## sample mean
## sample median
## sample 100th percentile
## sample quartiles

## sample variance
## sample standard deviation
## sample range
## sample interquartile range

## NOTE: Example 5
## Data which show the effect of two soporific drugs (increase in hours of sleep
## compared to control) on 10 patients. The group variable name may be misleading
## about the data: They represent measurements on 10 persons, not in groups.
## extra = increase in hours of sleep
## group = drug given
## ID = person ID
d <- as.data.table(sleep)

ggplot(d, aes(x=ID, y=extra, colour=group)) +
  geom_point() +
  theme(aspect.ratio = 1)

ggplot(d, aes(x=ID, y=extra, fill=group)) +
  geom_bar(
    stat='identity',
    position = 'dodge') +
  theme(aspect.ratio = 1)

ggplot(d, aes(x=extra, fill=group, alpha=0.25)) +
  geom_histogram(
    breaks=seq(-1,4,by=.1)
  ) +
  geom_density()

dd <- d[, mean(extra), .(group)]
ggplot(dd, aes(x=group, y=V1)) +
  geom_bar(
    stat='identity',
    col='black',
    fill='white') +
  theme(aspect.ratio=1)


## NOTE: Example 6
## ChickWeight
## The body weights of the chicks were measured at birth and every second day
## thereafter until day 20. They were also measured on day 21. There were four
## groups on chicks on different protein diets.

## weight:
## a numeric vector giving the body weight of the chick (gm).

## Time:
## a numeric vector giving the number of days since birth when the measurement
## was made.

## Chick:
## an ordered factor with levels 18 < … < 48 giving a unique identifier for the
## chick. The ordering of the levels groups chicks on the same diet together and
## orders them according to their final weight (lightest to heaviest) within
## diet.

## Diet:
## a factor with levels 1, …, 4 indicating which experimental diet the chick
## received.

d <- as.data.table(ChickWeight)

ggplot(d, aes(x=Time, y=weight, fill=Chick)) +
  geom_line() +
  facet_wrap(~Diet)

dd <- d[, mean(weight), .(Time, Diet)]
ggplot(dd, aes(x=Time, y=V1, col=Diet)) +
  geom_line()

## Lets insepct final weights
dd <- d[, weight[.N], .(Chick, Diet)]
ggplot(dd, aes(x=V1)) +
  geom_histogram(aes(y=..density..)) +
  geom_density() +
  facet_wrap(~Diet) +
  theme(aspect.ratio = 1)


## NOTE: Example 7
d <- as.data.table(iris)

ggplot(d, aes(x=Sepal.Length)) +
  geom_histogram(aes(y=..density..)) +
  geom_density() +
  facet_wrap(~Species) +
  theme(aspect.ratio = 1)

## Convert to long format
dd <- melt(d, id.vars='Species')

ggplot(dd, aes(x=value, fill=Species, alpha=0.25)) +
  geom_histogram(
    aes(y=..density..),
    col='black') +
  geom_density() +
  facet_wrap(~variable, ncol=4) +
  theme(aspect.ratio = 1)


## NOTE: Example 8
## USArrests
## Murder: murder arrests per 100,000
## Assault: assault arrests per 100,000
## Rape: rape arrests per 100,000
## UrbanPop: percent urban population
d <- as.data.table(USArrests)
dd <- melt(d, measure.vars=c('Murder', 'Assault', 'Rape'))

ggplot(dd, aes(x=UrbanPop, y=value)) +
  geom_point() +
  facet_wrap(~variable) +
  theme(aspect.ratio = 1)

