library(data.table)
library(ggplot2)

## Better intro to R motivation
## NOTE: the point of this next bit is to dazzle, not to teach
n <- 100
condition <- rep(c(1,2,3), each=n)
x1 <- rnorm(n, mean=25, sd=25)
x2 <- rnorm(n, mean=50, sd=15)
x3 <- rnorm(n, mean=75, sd=5)
y1 <- rnorm(n, mean=75, sd=25)
y2 <- rnorm(n, mean=50, sd=15)
y3 <- rnorm(n, mean=25, sd=5)

x <- c(x1, x2, x3)
y <- c(y1, y2, y3)

d <- data.table(condition, x, y)

ggplot(d, aes(x=x, y=y, colour=factor(condition))) +
  geom_point() +
  theme(aspect.ratio = 1) +
  facet_wrap(~condition)


ggplot(d, aes(x=x, y=y, colour=factor(condition))) +
  geom_point() +
  theme(aspect.ratio = 1)


## Any progress on sorting data.tables?
d
d[order(x)]
d[order(-y)]
d[order(x,-y)]

## Difference between = and <-???
