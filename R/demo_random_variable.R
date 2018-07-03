library('data.table')
library('ggplot2')

rm(list = ls())

## NOTE: Much -- if not all -- of the following material is dealing at the level
## of population. That is, we are not dealing with datasets and trying to infer
## population level knowledge. Rather, we are given some form of population
## knowledge, asked to compute other population level knowledge from that.


## NOTE: Example 1
## Suppose that two products A and B are judged by four consumers who then
## express a preference for A or B. We denote each outcome as a four element
## string, with each element indicating the preference of a consumer (e.g., ABAB
## denotes an outcome where the first and third consumer prefer product A and
## the second and fourth consumer prefer product B).

## Get every possible outcome (i.e., define the sample space):
possible_outcomes <- list(
c('A','A','A','A'),
c('A','A','A','B'),
c('A','A','B','A'),
c('A','A','B','B'),
c('A','B','A','A'),
c('A','B','A','B'),
c('A','B','B','A'),
c('A','B','B','B'),
c('B','A','A','A'),
c('B','A','A','B'),
c('B','A','B','A'),
c('B','A','B','B'),
c('B','B','A','A'),
c('B','B','A','B'),
c('B','B','B','A'),
c('B','B','B','B')
)

## convert to data.table form, because everything is better that way
d <- data.table(matrix(unlist(possible_outcomes), ncol=4, byrow=TRUE))
d[, ev := 1:.N]

## Now suppose that the products are alike in quality and that the consumers are
## express their preferences independently. Then the 16 simple events in the
## sample space are equally likely, and each has a probability of 1/16. Let a random variable be defined as:
## X = number of persons preferring A to B

## Add X to our data.table
d[, X := sum(.SD == 'A'), .(ev)]

## write down the probability distribution of X in table form:
table(d[, X])

## That last line was close, but not quite there. The issue is that table() just
## ocunts up the number of occurances of each unique outcome. We need to convert
## these counts into probabilities. To do this, we must divide each outcome by
## the total number of possible outcomes.
p <- table(d[, X]) / d[, .N]

## Find p(X >= 2)
p[3:5]
sum(p[3:5])

## Find p(1 <= X <= 3)
p[2:4]
sum(p[2:4])

## plot the probability distribution
dd <- data.table(p)
ggplot(dd, aes(x=V1, y=N)) +
  geom_point() +
  geom_segment(aes(xend=V1, yend=0)) +
  xlab('X = number of A preferences') +
  ylab('Probability') +
  theme(aspect.ratio = 1)


## NOTE: Example 2
## A large store places its last 15 clock radios in a clearance sale. Unknown to
## anyone, 5 of these radios are defective. If a customer tests 3 different
## clock radios selected at random, what is the distribution of:
## X = number of defective radios in the sample?

## Here, it is clear that there are only 4 distinct possible outcomes:
## 0, 1, 2, and 3

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

## plot the prob dist
d <- data.table(p)
d[, x := 0:3]
ggplot(d, aes(x=x, y=p)) +
  geom_point() +
  geom_segment(aes(xend=x, yend=0)) +
  xlab('X = number of defective radios in the sample') +
  ylab('Probability') +
  theme(aspect.ratio = 1)


## NOTE: Example 3
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

## Compute p(x>=3):
## Note that the first arg below is 1 (instead of the 2 that you might expect).
## This is because pgeom() starts counting from 0, not 1.
pgeom(1, 1/3, lower.tail=FALSE)

## plot geometric probability distribution
x <- 0:10
y <- dgeom(x, 1/3)
d <- data.table(x,y)
ggplot(d, aes(x=x,y=y)) +
  geom_point() + # we use point because it's a discrete distribution
  xlab('X = number trials until first success') +
  ylab('Probability') +
  theme(aspect.ratio = 1)

## draw a sample from the above geometric distribution and plot it with a
## histogram
## TODO: Play with "n"
n <- 100000
sample <- rgeom(n, 1/3)
d <- data.table(sample)
b <- seq(0.1,10,1)
ggplot(d, aes(sample)) +
  geom_histogram(aes(y=..density..),breaks=b) +
  theme(aspect.ratio = 1)

## Histogram really wasn't the best thing to use here... that's the reason for
## weird breaks. Histograms are well suited for continuous random variables, but
## not so much for discrete random variables.
dd <- data.table(table(d))
setnames(dd, 'd', 'x')
dd[, p_est := N / n]
dd[, x := as.integer(x)]
ggplot(dd, aes(x=x, y=p_est)) +
  geom_bar(stat='identity') +
  theme(aspect.ratio = 1)


## NOTE: Example 4
## A store manager uses extensive sales records to assess the probability distributions for:
## X = number of units of brand A sold in a week
## Y = number of units of brand B sold in a week
## These distributions are given by the following table:

## | Brand A | Value x          |   0 |   1 |   2 |   3 |   4 |   5 |
## | Brand A | Probability f(x) |  .1 |  .1 |  .2 |  .3 |  .2 |  .1 |
## |---------+------------------+-----+-----+-----+-----+-----+-----|
## | Brand B | Value y          |   0 |   1 |   2 |   3 |   4 |   5 |
## | brand B | Probability f(y) | .23 | .48 | .29 | 0.0 | 0.0 | 0.0 |

## Plot probability histograms of X and Y:

## We begin, as usual, by converting table to data.table
value <- c(0,1,2,3,4,5)
p_x <- c(.1,.1,.2,.3,.2,.1)
p_y <- c(.23,.48,.29, 0.0, 0.0, 0.0)
d <- data.table(value, p_x, p_y)

## This data.table is in "wide format"
## Convert to "long format"
dd <- melt(d, id.vars=1, measure.vars=2:3, value.name='p')

ggplot(dd, aes(x=value, y=p)) +
  geom_point() +
  geom_segment(aes(xend=value, yend=0)) +
  facet_wrap(~variable) +
  theme(aspect.ratio = 1)

## Brand A has more spread (variance) than brand B, and its central tendency
## (mean) appears to be something like 2 or 3, wheras brand B has a mean of 1 or
## so. The mean of a distribution is also called the "expected value" of a
## distribution. Recall that it is computed:

## x1*p(x1) + x2*p(x2) + ... + xn*p(xn)

## Compute expected value of X and Y
E_x <- 0*.1 + 1*.1 + 2*.2 + 3*.3 + 4*.2 + 5*.1
E_y <- 0*.23 + 1*.48 + 2*.29 + 3*0 + 4*0 + 5*0

## We can do this more efficiently by using the data.table
dd[, sum(value*p), .(variable)]

## Actually, there is a subtle yet gigantic difference between the means we
## computed in the descriptive statistics lectures and homeworks, and the
## expected values we are discussing here. That big difference is the difference
## between a sample and a population. Briefly:

## TODO: There is a lot here. Is it adequately treated?
## sample = some numbers that came from an experiment
## population = the distribution that generated those numbers

## The expected value of a distribution is also called the "population mean."
## mean(data) is the sample mean of some data. It is an **estimate** for the
## population mean. Here, we are given true p's, not data.


## NOTE: Example 5
## Let X be the number of brand A hi-fi units sold in a week whose probability distribution is given by:

## X = number of units of brand A sold in a week
## Y = number of units of brand B sold in a week

## | Brand A | Value x          |   0 |   1 |   2 |   3 |   4 |   5 |
## | Brand A | Probability f(x) |  .1 |  .1 |  .2 |  .3 |  .2 |  .1 |
## |---------+------------------+-----+-----+-----+-----+-----+-----|
## | Brand B | Value y          |   0 |   1 |   2 |   3 |   4 |   5 |
## | brand B | Probability f(y) | .23 | .48 | .29 | 0.0 | 0.0 | 0.0 |

## Suppose that a profit of $50 is realized on each unit sold and that the
## weekly fixed cost is $20.

## What is the expected net profit?

## Begin by copying relevant lines of code from above:
## We begin, as usual, by converting table to data.table
value <- c(0,1,2,3,4,5)
p_x <- c(.1,.1,.2,.3,.2,.1)
p_y <- c(.23,.48,.29, 0.0, 0.0, 0.0)
d <- data.table(value, p_x, p_y)

## This data.table is in "wide format"
## Convert to "long format"
dd <- melt(d, id.vars=1, measure.vars=2:3, value.name='p')

## Expected net profit:
## (profit per sale) * (expected number of units sold) - (weekly fixed cost)
dd[, 50*sum(value * p) - 20, variable]


## NOTE: Variance - A measure of spread
## Population Variance = The expected squared deviation from the mean
## Var(X) = E((X - E(X))^2)
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

## NOTE: Example 6
## Comptue the (population) variance and standard deviation of X and Y from the
## above example.

## X = number of units of brand A sold in a week
## Y = number of units of brand B sold in a week

## | Brand A | Value x          |   0 |   1 |   2 |   3 |   4 |   5 |
## | Brand A | Probability f(x) |  .1 |  .1 |  .2 |  .3 |  .2 |  .1 |
## |---------+------------------+-----+-----+-----+-----+-----+-----|
## | Brand B | Value y          |   0 |   1 |   2 |   3 |   4 |   5 |
## | brand B | Probability f(y) | .23 | .48 | .29 | 0.0 | 0.0 | 0.0 |

## Begin by copying relevant lines of code from above:
## We begin, as usual, by converting table to data.table
value <- c(0,1,2,3,4,5)
p_x <- c(.1,.1,.2,.3,.2,.1)
p_y <- c(.23,.48,.29, 0.0, 0.0, 0.0)
d <- data.table(value, p_x, p_y)

## This data.table is in "wide format"
## Convert to "long format"
dd <- melt(d, id.vars=1, measure.vars=2:3, value.name='p')

## Lets take an iterative approach
## start by computing the expected value (mean)
dd[, pop_mean := sum(value*p), .(variable)]

## Now compute variance
dd[, pop_var := sum((value^2)*p) - pop_mean^2, .(variable)]

## Compute standard deviation (sd = sqrt(var))
dd[, pop_sd := sqrt(pop_var)]


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
