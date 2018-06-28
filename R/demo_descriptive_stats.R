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

## add measures of central tendency to the plot
## add the sample mean and the sample median to the plot

## "mean" is pretty much another word for "average"
## "median" just means the value the splits the data into two even groups

## mean = sum(all observations) / (number of observations)
## median = (split the data into two even groups)

## Also note the use of the word "sample" above.
## Sample just means that we are computing descriptive statistics about the data
## we observed. It's an important thing to specify because -- in inferential
## statistics -- we try to guess things about what future samples might look
## like, and these guesses use very similar / identical words. The magic words
## that we use to distinguish these worlds are:
## "sample" mean refers to the data we observe
## "population" mean refers to our guesses
## Note that we will refine these definitions as the course proceeds

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

## NOTE: The take-home message for mean vs median is that the mean is more
## sensitive to outliers than the median


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

## Before prepping for this class, I'd never seen a dotplot. In my experience, a
## histogram is by far the more common diagnostic tool.
ggplot(d, aes(x=ozone)) +
  geom_histogram(
    col='black',
    fill='white') +
  scale_y_continuous('count', 1:10) +
  theme(aspect.ratio = 1)

## investigate the correspondence between dotplots and histograms
ggplot(d, aes(x=ozone)) +
  geom_histogram(
    col='black',
    fill='white') +
  geom_dotplot() +
  scale_y_continuous('count', 1:10) +
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

## This is a fine time to begin examing "spread"
## Add sample variance, standard deviation, and range to the plot

## sample variance =
## Step 1: Calculate the mean (the average weight).
## Step 2: Subtract the mean and square the result.
## Step 3: Work out the average of those differences.

## sample standard deviation = sqrt(sample variance)

## sample range = difference between most extreme values
ozone_mean<- d[, mean(ozone)]
ozone_median <- d[, median(ozone)]
ozone_var <- d[, var(ozone)]
ozone_sd <- d[, sd(ozone)]
ozone_range <- d[, diff(range(ozone))]

b <- seq(0,12,by=1.0)
ggplot(d, aes(x=ozone)) +
  geom_histogram(
    aes(y=..density..),
    breaks=b,
    col='black',
    fill='white') +
  geom_density(colour='black', size=1.25) +
  theme(aspect.ratio = 1) +
  geom_vline(xintercept = ozone_mean, colour='black') +
  geom_segment(
    aes(x=ozone_mean, xend=ozone_mean+ozone_var, y=.3, yend=.3),
    colour='red') +
  geom_segment(
    aes(x=ozone_mean, xend=ozone_mean-ozone_var, y=.3, yend=.3),
    colour='red') +
  geom_segment(
    aes(x=ozone_mean, xend=ozone_mean+ozone_sd, y=.35, yend=.35),
    colour='blue') +
  geom_segment(
    aes(x=ozone_mean, xend=ozone_mean-ozone_sd, y=.35, yend=.35),
    colour='blue') +
  geom_segment(
    aes(x=ozone_mean, xend=ozone_mean+ozone_range, y=.4, yend=.4),
    colour='green') +
  geom_segment(
    aes(x=ozone_mean, xend=ozone_mean-ozone_range, y=.4, yend=.4),
    colour='green')


## NOTE: Example 4
## Data which show the effect of two soporific drugs (increase in hours of sleep
## compared to control) on 10 patients. The group variable name may be misleading
## about the data: They represent measurements on 10 persons, not in groups.
## extra = increase in hours of sleep
## group = drug given
## ID = person ID
d <- as.data.table(sleep)

## If you really needed sleep, which drug would you take?

## This plot might suggest drug 2
ggplot(d, aes(x=ID, y=extra, fill=group)) +
  geom_bar(
    stat='identity',
    position = 'dodge') +
  theme(aspect.ratio = 1)

## There's clearly lots of overlap and noise, but if I had to choose one,
## I'd still choose drug 2.
extra_mean_1 <- d[group==1, mean(extra)]
extra_sd_1 <- d[group==1, sd(extra)]
extra_mean_2 <- d[group==2, mean(extra)]
extra_sd_2 <- d[group==2, sd(extra)]
ggplot(d, aes(x=extra, fill=group, alpha=0.9)) +
  geom_histogram(
    aes(y=..density..),
    breaks=seq(-1,4,by=.1)
  ) +
  theme(aspect.ratio = 1) +
  geom_density() +
  geom_vline(xintercept = extra_mean_1, colour='red') +
  geom_vline(xintercept = extra_mean_2, colour='blue') +
  geom_segment(
    x=extra_mean_1,
    xend=extra_mean_1+extra_sd_1,
    y=1.5,
    yend=1.5,
    colour='red') +
  geom_segment(
    x=extra_mean_1,
    xend=extra_mean_1-extra_sd_1,
    y=1.5,
    yend=1.5,
    colour='red') +
  geom_segment(
    x=extra_mean_2,
    xend=extra_mean_2+extra_sd_2,
    y=1.6,
    yend=1.6,
    colour='blue') +
  geom_segment(
    x=extra_mean_2,
    xend=extra_mean_2-extra_sd_2,
    y=1.6,
    yend=1.6,
    colour='blue')

## Often, you won't see histograms in papers. Rather, you'll see bar graphs with
## error bars. The height of the bar shows the mean observed value, and the
## error bars often show somethign called SEM (standard error of the mean). We
## will get to that later. For now, lets just show standard deviations.
dd <- d[, .(mean(extra), sd(extra)), .(group)]
ggplot(dd, aes(x=group, y=V1)) +
  geom_bar(
    stat='identity',
    col='black',
    fill='white') +
  geom_errorbar(aes(x=group,
                    ymin=V1+V2,
                    ymax=V1-V2), width=.2) +
  theme(aspect.ratio=1)

## NOTE: Example 5
## The UCBAdmissions comes built in with R.
## This data set is frequently used for illustrating Simpson's paradox, see
## Bickel et al (1975). At issue is whether the data show evidence of sex bias
## in admission practices. E.g., are males or females more likely to be admitted
d <- as.data.table(UCBAdmissions)

## Lets start with the basics by plotting a histogram
## Just in case it hasn't sunk in yet, histograms answer the question: How is
## our data **distributed**

## Since we asked a question about **admission** lets start by only
## looking at the number of males and females admitted (as opposed to rejected).

## Use a histogram to examine the distribution across departments of number of
## males and females admitted
b <- seq(0,500,50)
ggplot(d[Admit=='Admitted'], aes(x=N, fill=Gender, alpha=0.1)) +
  geom_histogram(aes(y=..density..), breaks=b) +
  geom_density() +
  theme(aspect.ratio = 1)

## Q: What is the above histogram really showing? A: You can think of it as
## the probability distribution (which we will cover formally next week) that a
## random department will admit N students

## Q: If you didn't know anything else, and you only had this histogram, how
## many males and females would you guess will be accepted the next time around?

## So, more males tend to be accepted? Is it settled?
## What if more males are also rejected?
b <- seq(0,500,50)
ggplot(d, aes(x=N, fill=Gender, alpha=0.1)) +
  geom_histogram(aes(y=..density..), breaks=b) +
  geom_density() +
  facet_wrap(~Admit) +
  theme(aspect.ratio = 1)

## Together, these histograms frame an interesting question... it's interesting
## because the answer to our question isn't totally clear just by looking at
## it... at least not yet. What can we do to clarify the situation?

## Making the simplest possible plot is always a good thing to explore
dd <- d[, .(mean(N), sd(N)), .(Gender, Admit)]
setnames(dd, c('V1', 'V2'), c('N_mean', 'N_err'))
ggplot(dd, aes(x=Gender, y=N_mean)) +
  geom_bar(stat='identity') +
  geom_errorbar(aes(ymin=N_mean-N_err, ymax=N_mean+N_err, width=0.25)) +
  facet_wrap(~Admit) +
  theme(aspect.ratio=1)

## Woah, thoser error bars are enormous.
## What is the point of error bars anyway?
## Useful error bars tell us something about the range of values future data is
## likely to fall within. Turns out, standard error (we will discuss formally
## next week) does a better job of this than straight sd.
## SEM = sd / sqrt(N)
dd <- d[, .(mean(N), sd(N)/sqrt(.N)), .(Gender, Admit)]
setnames(dd, c('V1', 'V2'), c('N_mean', 'N_err'))
ggplot(dd, aes(x=Gender, y=N_mean)) +
  geom_bar(stat='identity') +
  geom_errorbar(aes(ymin=N_mean-N_err, ymax=N_mean+N_err, width=0.25)) +
  facet_wrap(~Admit) +
  theme(aspect.ratio=1)

## Hmm. I'm still having a hard time making a decision. Looks like more males
## are accepted, but more males applied overall. Seems like what we need is a
## way to control for the number of applicants overall. We do this next.

## We are asked if the probability of being admitted is different for males and
## females. We're going to get much deeper into probability next week, but for
## now, its safe to go with our intuition. Probability is how likely something
## is to occur, and in the absence of other information, how likely something is
## to occur can be estimated by the proportion of times it did occur in some
## data (which we will call a "sample"). Lets compute these proportions for the
## present question.
dd <- d[,
        N[Admit=='Admitted'] / (N[Admit=='Admitted'] + N[Admit=='Rejected']),
        .(Dept, Gender)]
setnames(dd, 'V1', 'p_admit')

## Notice the following plot does not include error bars... Why?
ggplot(dd, aes(x=Gender, y=p_admit)) +
  geom_bar(stat='identity') +
  facet_wrap(~Dept) +
  theme(aspect.ratio=1)

## examine the mean p_admit across all departments
ddd <- dd[, .(mean(p_admit), sd(p_admit)/sqrt(.N)), .(Gender)]
setnames(ddd, c('V1','V2'), c('p_mean', 'p_err'))
ggplot(ddd, aes(x=Gender, y=p_mean)) +
  geom_bar(stat='identity') +
  geom_errorbar(aes(ymin=p_mean-p_err, ymax=p_mean+p_err, width=0.25)) +
  theme(aspect.ratio=1)

## The last plot suggests no difference... hmm. So is there or isn't there?
## In this situation -- which is most situations -- we will really only be
## looking to see that you have inspected the data in a variety of ways (e.g.,
## different plot types, collapsing across different variables, etc.), and are
## doing your best to reason about the data. Things will get more formal in
## later lectures (e.g., when we cover Null Hypothesis Significance Testing).


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
## a factor with levels 1 thru 4 indicating which experimental diet the chick
## received.

d <- as.data.table(ChickWeight)

## begin by plotting x=time y=weight for all rats separately and grouped by diet
ggplot(d, aes(x=Time, y=weight, fill=Chick)) +
  geom_line() +
  facet_wrap(~Diet)

## TODO: Let try these as sorta quick in-class exercises
## plot mean of all rats per groupd
dd <- d[, mean(weight), .(Time, Diet)]
ggplot(dd, aes(x=Time, y=V1, col=Diet)) +
  geom_line()

## add error bars
dd <- d[, .(mean(weight), sd(weight)/sqrt(.N)), .(Time, Diet)]
setnames(dd, c('V1','V2'), c('weight_mean', 'weight_err'))
ggplot(dd, aes(x=Time, y=weight_mean, col=Diet)) +
  geom_line() +
  geom_errorbar(aes(ymin=weight_mean-weight_err, ymax=weight_mean+weight_err))

## Lets inspect the distribution of final weights
## plot histogram of final weights seperately for each group
dd <- d[, weight[.N], .(Chick, Diet)]
ggplot(dd, aes(x=V1)) +
  geom_histogram(aes(y=..density..)) +
  geom_density() +
  facet_wrap(~Diet) +
  theme(aspect.ratio = 1)

## Inspect the distribution of final weights using a box plot
dd <- d[, weight[.N], .(Chick, Diet)]
ggplot(dd, aes(x=Diet, y=V1)) +
  geom_boxplot()

## Inspect the final weights using a bar graph with error bars
dd <- d[, weight[.N], .(Chick, Diet)]
ddd <- dd[, .(mean(V1), sd(V1)/sqrt(.N)), .(Diet)]
setnames(ddd, c('V1','V2'), c('mean','err'))
ggplot(ddd, aes(x=Diet, y=mean)) +
  geom_bar(stat='identity') +
  geom_errorbar(aes(ymin=mean-err, ymax=mean+err, width=0.25))

## Which feed would you choose?
## How confident are you in your choice?
## Try to frame your answer in terms of likelihood, probability and the plots we
## just made
## Would your decision be different at day 10 than at day 20?

## What do you think the distribution of weights would look like at day 30?


## NOTE: Example 7
## This famous (Fisher's or Anderson's) iris data set gives the measurements in
## centimeters of the variables sepal length and width and petal length and
## width, respectively, for 50 flowers from each of 3 species of iris. The
## species are _Iris setosa_, _versicolor_, and _virginica_.

## ‘iris’ is a data frame with 150 cases (rows) and 5 variables (columns) named
## ‘Sepal.Length’, ‘Sepal.Width’, ‘Petal.Length’, ‘Petal.Width’, and ‘Species’.

d <- as.data.table(iris)

## Use a histogram to inspect the distrbution of sepal length separately for
## each species.
ggplot(d, aes(x=Sepal.Length)) +
  geom_histogram(aes(y=..density..)) +
  geom_density() +
  facet_wrap(~Species) +
  theme(aspect.ratio = 1)

## Use a histogram to inspect the distribution of every dependent variable
## (‘Sepal.Length’, ‘Sepal.Width’, ‘Petal.Length’, and ‘Petal.Width’).

## How the heck are we going to do this with d in it's current format?
## What format do we want / need it in to make it play nice with ggplot?

## Convert to long format
dd <- melt(d, id.vars='Species')

ggplot(dd, aes(x=value, fill=Species, alpha=0.25)) +
  geom_histogram(
    aes(y=..density..),
    col='black') +
  geom_density() +
  facet_wrap(~variable, ncol=4) +
  theme(aspect.ratio = 1)

## inspect the distribution with boxplots instead of histograms
ggplot(dd, aes(x=Species, y=value)) +
  geom_boxplot() +
  facet_wrap(~variable, ncol=4) +
  theme(aspect.ratio = 1)

## TODO: possibly a homework problem --- bar graph with error bars Do you think
## there are any significant differences between species according to
## ‘Sepal.Length’, ‘Sepal.Width’, ‘Petal.Length’, or ‘Petal.Width’?

## TODO: If you go out and observe an iris of each of the 3 species discussed above,
## what values of sepal length, sepal width, petal length, and petal width do
## you think you are most likely to observe?
