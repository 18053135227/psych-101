## NOTE: 1
## Each week a grocery shopper buys either canned (C) or bottled (B) soft
## drinks. The type of soft drink purchased in 4 consecutive weeks is to be
## recorded.

## (a) List the sample space
e <- c('C','B')
ss <- permutations(length(e), 4, e, repeats.allowed=TRUE)

## (b) If a different type of soft drink is purchased than in the previous week,
## we say there is a switch. Let,

## X = number of switches

## Determine the value of X for each elementary outcome (e.g, BBBB corresponds
## to x=0; BCBC corresponds to x=3).
d <- data.table(matrix(unlist(ss), ncol=4, byrow=TRUE))
d[, row := 1:.N]

d[, V1 := ifelse(V1=='C', 0, 1)]
d[, V2 := ifelse(V2=='C', 0, 1)]
d[, V3 := ifelse(V3=='C', 0, 1)]
d[, V4 := ifelse(V4=='C', 0, 1)]

d[, V1V2 := V1-V2]
d[, V2V3 := V2-V3]
d[, V3V4 := V3-V4]

d[, X := sum(c(V1V2!=0, V2V3!=0, V3V4!=0)), .(row)]

## (c) Suppose that for each purchase P(B)=0.5 and the decisions in different
## weeks are independent. Assign probabilities to the elementary outcomes and
## obtain the distribution of X.

## Because each choice is independent, the probability of asequence of any four
## choices being made is the same: 0.5*0.5*0.5*0.5
d[, p := (.5)^4]

## verify that p sums to 1
d[, sum(p)]

## What unique outcomes does X have?
d[, unique(X)]

## To get the distribution for X, we want to sum up the probability for each
## unique outcome.
px <- d[, sum(p), .(X)]

## just for giggles, lets plot it
ggplot(px, aes(x=X, y=V1)) +
  geom_point() +
  geom_segment(aes(xend=X, yend=0)) +
  xlab('X = Number of choice switches') +
  ylab('Probability')


## NOTE: 5
## In a study of the coexistence of two types of insect, let X and Y denote the
## number of type A and B insects, respectively, that reside on the same plant.
## From observations of a large number of of plants, suppose that the following
## joint probability distribution is assessed for the insect counts per plant.

## | y\x |   1 |   2 |   3 |   4 |
## |-----+-----+-----+-----+-----|
## |   0 |   0 | .05 | .05 | .10 |
## |   1 | .08 | .15 | .10 | .10 |
## |   2 | .20 | .12 | .05 |   0 |

x <- rep(c(1,2,3,4), 3)
y <- rep(c(0,1,2), each=4)
p <- c(0,.05,.05,.10,.08,.15,.10,.10,.20,.12,.05,0)

d <- data.table(x,y,p)

## (a) Find the probability that there are more type B insects than type A
## insects on a plant.
d[y>x, sum(p)]

## (b) Compute E(X), E(Y), Var(X), Var(Y), and Cov(X,Y)
Ex <- d[, sum(x*p)]
Ey <- d[, sum(y*p)]
varx <- d[, sum((x^2)*p) - Ex^2]
vary <- d[, sum((y^2)*p) - Ey^2]
covxy <- d[, sum((x*y)*p) - Ey*Ex]

## lets plot this to see if our answers make any sense
ggplot(d, aes(x=x, y=p)) +
  geom_bar(stat='sum')

ggplot(d, aes(x=y, y=p)) +
  geom_bar(stat='sum')

ggplot(d, aes(x=x, y=y)) +
  geom_tile(aes(fill=p))

## (c) Find the correlation coefficient between X and Y. Interpret the result.
corrxy <- covxy / (sqrt(varx)*sqrt(vary))

## (d) Let the total number of insects living on a plant be:
## T = X + Y
## Obtain the probability distribution of T and use it to calculate E(T) and Var(T)
d[, t := x + y]
Et <- d[, sum(t*p)]
vart <- d[, sum((t^2)*p) - Et^2]

## (e) Verify:
## (i) E(T) = E(X) + E(Y)
## (ii) Var(T) = Var(X) + Var(Y) + 2cov(X,Y)
Et == (Ex + Ey)
vart == (varx + vary + 2*covxy)
## TODO: I did something wrong... figure it out


## NOTE: 6
## Consider the joint distribution

## | x\y |  -1 |   0 |   1 |
## |-----+-----+-----+-----|
## |   0 |   0 | 1/3 |   0 |
## |   1 | 1/3 |   0 | 1/3 |

x <- rep(c(0,1), each=3)
y <- rep(c(-1,0,1), 2)
p <- c(0,1/3,0,1/3,0,1/3)
d <- data.table(x,y,p)

## (a) Show that X and Y are not independent
d[, px := sum(p), .(x)]
d[, py := sum(p), .(y)]

## if X and Y are independent then P(x)==P(y) for all x and y.
## This is clearly not true, so X and Y are not independent
d[, px==py]

## (b) Show that Corr(X,Y)=0
Ex <- d[, sum(x*p)]
Ey <- d[, sum(y*p)]
varx <- d[, sum((x^2)*p) - Ex^2]
vary <- d[, sum((y^2)*p) - Ey^2]
covxy <- d[, sum((x*y)*p) - Ey*Ex]
corrxy <- covxy / (sqrt(varx)*sqrt(vary))

