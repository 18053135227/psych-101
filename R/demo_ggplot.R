library(data.table)
library(ggplot2)

rm(list=ls())

## NOTE: 1
## http://rstudio-pubs-static.s3.amazonaws.com/232578_6e98a711d3854ce0bb2f8a996b804cf7.html#

?mpg
## A data frame with 234 rows and 11 variables

## manufacturer
## model
## model name

## displ
## engine displacement, in litres

## year
## year of manufacture

## cyl
## number of cylinders

## trans
## type of transmission

## drv
## f = front-wheel drive, r = rear wheel drive, 4 = 4wd

## cty
## city miles per gallon

## hwy
## highway miles per gallon

## fl
## fuel type

## class
## "type" of car

d <- as.data.table(mpg)

ggplot(d, aes(x = displ, y = hwy)) +
  geom_point()

## Colour, Size, Shape and other aesthetic attributes
## Colour can be used for analysing the outliers, the deviations from the
## general trend where mileage decreases with engine size, here applied to the
## city mileage (cty):
ggplot(mpg, aes(displ, cty, colour = class)) +
  geom_point()

## Facetting
## An alternative to aesthetics other than x and y axes is facetting.
ggplot(mpg, aes(displ, hwy)) +
  geom_point() +
  facet_wrap(~class)

## Plot geoms
## Plotting points is a but one way to visualise data. Many other exist, such as
## line graphs and box plots. An important class are smoothers (Note the relation
## to EDA terminology: smooths and roughs!)
## Adding a smoother to a plot
ggplot(mpg, aes(displ, hwy)) +
  geom_point() +
  geom_smooth()

## The smoother by default is loess, which is non-linear. For fiting with a linear
## function, do this:
ggplot(mpg, aes(displ, hwy)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

## Boxplots and jittering
## When combining a continuious variable, such as highway mileage, with a
## categorical one, such as engine type, we are usually looking at the
## categorical variable as a level:
ggplot(mpg, aes(drv, hwy)) +
  geom_point()

## This may lead to the data points becoming overprinted, as shown above: There are
## many more points plotted than visible. There are three ways to avoid that:
ggplot(mpg, aes(drv, hwy)) +
  geom_jitter()

ggplot(mpg, aes(drv, hwy)) +
  geom_boxplot()

ggplot(mpg, aes(drv, hwy)) +
  geom_violin()


## NOTE: 2
## https://rpubs.com/krask/98655
## Exercise 1: The diamonds data.frame is included in the ggplot2 package. Study
## the relationships between caret, price and color. We know that price depends
## somewhat on carat, but does this dependence vary by color? Produce the plot or
## plots that you feel best communicates the relationship and then describe in
## words what you see.
str(diamonds)
table(diamonds$color)

d <- as.data.table(diamonds)

## Since color is a factor variable with 7 levels, we can plot carat vs. price with
## color represented by various colors.
ggplot(data = d, aes(carat, price, color)) +
  geom_point(aes(color = color), size = 1)

## deal with visual overload by only plotting a small sample of avaiable data
set.seed(123)
dd <- d[sample(1:.N, 100)]
ggplot(data = dd, aes(carat, price, color)) +
  geom_point(aes(color = color), size = 3)

## Now we add a differentiable curve and regression line.
ggplot(data = dd, aes(carat, price, color)) +
  geom_point(aes(color = color), size = 3) +
  geom_smooth()

ggplot(data = dd, aes(carat, price, color)) +
  geom_point(aes(color = color), size = 3) +
  geom_smooth(method = "lm", se = FALSE)

## We can observe an apparent linear relationship between price and carat but it is
## difficult to discern if any relationship exists between price and color with the
## above graphs. Therefore,we will explore the impact of color using geom_jitter
## using the full diamonds data frame.

## Note: We will explore the relationship color has with carat, price, as well as
## price/carat.

## For Color and Carat:
ggplot(data = d, aes(x = color, y = carat)) +
  geom_jitter(alpha = 0.1)

ggplot(data = d, aes(x = color, y = carat)) +
  geom_boxplot(aes(fill = color))

## From both of the above plots, we notice that there may be a relationship between
## color and carat with higher carats boasting a higher (i.e further from the start
## of the alphabet) color. To explore this, we can look at the density plot for
## carat with each color.
ggplot(data = d, aes(x = carat)) +
  geom_density(aes(color = color)) +
  scale_x_continuous(limits = c(0,3))

## The density plot is consistent with our earlier observation suggesting a
## relationship.

## Now we investigate the relationship between color and price.
## For Color and Price:
ggplot(data = d, aes(x = color, y = price)) +
  geom_jitter(alpha = 0.2, color = "blue")

ggplot(data = d, aes(x = color, y = price)) +
  geom_boxplot(aes(fill = color))

## Similar to our observations of color and carat, we notice that higher prices are
## associated with the colors further away from the start of the alphabet. This is
## what we might expect given our observations about the relationship between color
## and carat. The following density plot of price should also be consistent with
## the density plot of carat.
ggplot(data = d, aes(x = price)) +
  geom_density(aes(color = color)) +
  scale_x_continuous(limits = c(0,17000))

## Though not as clear, we see that colors D, E, F, and G are peaked at a lower
## price than the remaining colors of H, I, and J, suggesting a relationship
## between color and price.

## Since carat and price appear to have a linear relationship, we expect
## price/carat to illustrate a similar relationship. We plot the same graphs as
## above to be consistent.

## For Color and Price/Carat:
ggplot(data = d, aes(x = color, y = price/carat)) +
  geom_jitter(alpha = 0.2, color = "green")

ggplot(data = d, aes(x = color, y = price/carat)) +
geom_boxplot(aes(fill = color))

ggplot(data = d, aes(x = price/carat)) +
  geom_density(aes(color = color)) +
  scale_x_continuous(limits = c(0,12000))

## Based on all of the above plots, we observe that color does have a
## relationship with price and carat.


## NOTE: 3
## https://rpubs.com/krask/98655

## Exercise 2: This exercise uses the Houston flight data. Here, we’ll use a
## version of the data set that is available in the package hflights. Install the
## package hflights and then execute.
library(hflights)
str(hflights)

d <- as.data.table(hflights)

## Study the relationship between arrival delay (ArrDelay), arrival time (ArrTime)
## and day of week (DayOfWeek). Are delays more likely at certain times of day or
## days of week? Note: transform the data to not allow negative arrival delays.
ggplot(data = d, aes(x = factor(DayOfWeek), y = ArrDelay)) +
  geom_point() +
  xlab("Day of Week") +
  ylab("Arrival Delay")

ggplot(data = hflights, aes(x = factor(DayOfWeek), y = ArrDelay)) +
  geom_boxplot() +
  xlab("Day of Week") +
  ylab("Arrival Delay")

## Based on the above two plots, we do not see a discernable difference between
## arrival delays and the day of the week. At first glance, it appears as if there
## is no relationship between arrival delays and day of the week.
ggplot(data = hflights, aes(x = ArrDelay)) +
  geom_density(aes(color = factor(DayOfWeek))) +
  scale_x_continuous(limits = c(0,250))

## Based on the density plot it seems to confirm that there is not a relationship
## between arrival delays and day of the week.

## For Arrival Time:
ggplot(data = d, aes(x = ArrTime, y = ArrDelay)) +
  geom_jitter(alpha=0.1) +
  xlab("Arrival Time") +
  ylab("Arrival Delay")

ggplot(data = d, aes(x = ArrTime, y = ArrDelay)) +
  geom_point() +
  geom_smooth(method = "lm")

## We notice that the regression line is flat and both of the above graphs suggest
## that there is no relationship between arrival times and arrival delays.

## We will take a sample to declutter the graphs and confirm the lack of
## relationship betweent the variables.
set.seed(111)
dd <- d[sample(1:.N, 1000)]

ggplot(data = dd, aes(x = ArrTime, y = ArrDelay)) +
  geom_jitter() +
  xlab("Arrival Time") +
  ylab("Arrival Delay") +
  ggtitle("Random Sample")

ggplot(data = dd, aes(x = ArrTime, y = ArrDelay)) +
  geom_point() +
  geom_smooth(method = "lm")

## The sample confirms the appearance of no relationship between arrival delays and
## arrival times.
