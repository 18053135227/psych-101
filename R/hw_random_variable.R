library('data.table')
library('ggplot2')
library('gtools')

## NOTE: Math-y thing you will need for this homework
## Let Y ~ a*X + b
## Then:
## E(Y) = a*E(X) + E(b)
## Var(Y) = (a^2)*E(X)

## NOTE: 2
## For the following probability distribution:

## | x    |  2 |  3 |  4 |  5 |  6 |
## |------+----+----+----+----+----|
## | f(x) | .1 | .3 | .3 | .2 | .1 |

## (a) Plot the distribution
## (b) Calculate E(X)
## (c) Find P(X>=4) and P(2<X<=4)
## (d) Let Y = (2X-8)^2
## Obtain the distribution of Y and calculate E(Y)


## NOTE: 3
## For the following probability distribution:

## | x    |  0 |  1 |  2 |  3 |
## |------+----+----+----+----|
## | f(x) | .3 | .4 | .2 | .1 |

## Find:

## (a) P(X>=2)
## (b) P(0<X<=2)
## (c) E(X)
## (d) Var(X); sd(X)


## NOTE: 4
## For scores X on a nationally administered aptitude test, the mean and variance are:
## E(X)=120
## Var(X)=100

## Find the mean and variance of:
## (a) Y = (X-120)/10
## (b) Y = (X-100)/20


## NOTE: 5 - Geometric distribution
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

## The geometric distribution is defined by one parameter: The probability of
## success. Let P(Success) = 1/3.

## (a) Compute the following probabilities:
## p(X = 3)
## p(X > 3)
## p(X >= 3)
## p(X <= 3)
## p(X <= 3) + p(X > 3)

## (b) Plot geometric probability distribution for x = 0:10

## (c) Draw a sample of size n= 1000 from the above geometric distribution and
## plot it with a histogram

## (d) Plot the geometric distribution for the following values of P(Success):
## p(Success) = 0.2
## p(Success) = 0.5
## p(Success) = 0.8
## Use facet_wrap to plot on a single figure.

## (e) Compute the expected value of the geometric distributions that you
## plotted in (d). For the geometric distribution, x can assume any value from
## zero to infinity, but for now, assume x only ranges from 0 to 10. Add
## vertical lines to indicate the population mean to each panel of the figure
## you made in part (d).

## (f) It turns out it can be proven that if X ~ geometric with probability of
## success of p, then E(X) = (1-p)/p. How does this compare to the population
## means you computed in (e)? Are they different, and if so, why?

## (g) Plot samples from a geometric distribution for the following values of
## P(Success):
## p(Success) = 0.2
## p(Success) = 0.5
## p(Success) = 0.8
## Use facet_wrap to plot on a single figure.

## (h) For each panel in (f), draw a vertical line indicating the sample mean

## (i) Repeat (g) and (h) a few times to see the variation in sample mean, and
## the absence of variance in the population mean. Make a figure that shows the
## results of each of these experiments in separate panel of one figure.


## NOTE 6: A group of researchers is trying to determine the efficacy of an
## anti-depressant. To do so, they run an experiment in which they administer
## the anti-depressant to a series of rats until the first rat shows signs of
## cheering up. Suppose that the 10th rat to receive the treatment showed signs
## of reduced depression and the experiment was stopped.

## (a) Use null hypothesis significance testing to test:
## H0: p_success = 0.5
## H1: p_success > 0.5


## NOTE: 7
## Suppose we have a board game that depends on the roll of two die and attaches
## special importance to rolling snake eyes (1, 1).

## In a particular game, the two die are rolled 200 times, and snake eyes comes
## up 12 times. Is the proportion of snake eyes significantly higher than would
## be expected by chance assuming the die are fair?

## (a) answer this question without using the function binom.test()

## (b) use binom.test() to answer this question.
