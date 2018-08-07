library('data.table')
library('ggplot2')

rm(list=ls())

## NOTE: One-way ANOVA:
## | Treatment 1 | Treatment 2 | Treatment 3 |
## |-------------+-------------+-------------|
## | x1          | y1          | z1          |
## | x2          | y2          | z2          |
## | ...         | ...         | ...         |
## | xn          | yn          | zn          |
## |-------------+-------------+-------------|
## |             |             |             |

## between_group_variation:
## comparing group means against grand mean:
## (x_bar - grand_mean)^2 + (y_bar - grand_mean)^2 + (z_bar - grand_mean)^2

## within_group_variation:
## comparing within-group observations to group means:
## sum((x-x_mean)^2) + sum((y-y_mean)^2) + sum((z-z_mean)^2)


## NOTE: Two-way ANOVA:
## Let x_{i,j,k} indicate the kth measurement from condition i and block j
## | Condition 1 | Condition 1 | Condition 2 | Condition 2 |
## |-------------+-------------+-------------+-------------|
## | Block 1     | Block 2     | Block 1     | Block 2     |
## |-------------+-------------+-------------+-------------|
## | x111        | x121        | x211        | x221        |
## | x112        | x122        | x212        | x222        |
## | ...         | ...         | ...         | ...         |
## | x11n        | x12n        | x21n        | x22n        |
## |-------------+-------------+-------------+-------------|
## |             |             |             |             |

## NOTE: Between-group calculations:
## Condition: (x1.._bar - grand_mean)^2 + (x2.._bar - grand_mean)^2
## Block:     (x.1._bar - grand_mean)^2 + (x.2._bar - grand_mean)^2
## Condition*Block: (x11._bar - grand_mean)^2 +
##                  (x21._bar - grand_mean)^2 +
##                  (x12._bar - grand_mean)^2 +
##                  (x22._bar - grand_mean)^2 -
##                  between_sse_condition -
##                  between_sse_block

## NOTE: Within-group calculations:
## sse_within: sum((x11. - x11._bar)^2) +
##             sum((x12. - x12._bar)^2) +
##             sum((x21. - x21._bar)^2) +
##             sum((x22. - x22._bar)^2)

d <- fread('../data/rbbsdelay/rbbsdelay.csv')

## TODO: good opportunity to dicuss checking results for plausibility
d[, condition := factor(condition)]
d[, block := factor(block)]
d[, block := as.character(block)]

fm <- lm(acc_mean ~ condition*block, data=d)
anova(fm)

d[, grand_mean := mean(acc_mean)]
d[, condition_mean := mean(acc_mean), .(condition)]
d[, block_mean := mean(acc_mean), .(block)]
d[, interaction_mean := mean(acc_mean), .(condition, block)]

d[, between_sse_condition := sum((condition_mean - grand_mean)^2)]
d[, between_sse_block := sum((block_mean - grand_mean)^2)]
d[, between_sse_interaction := sum((interaction_mean - grand_mean)^2) -
      between_sse_condition - between_sse_block]

d[, within_sse := sum((acc_mean - interaction_mean)^2)]

d[, df_condition := length(unique(condition)) - 1]
d[, df_block := length(unique(block)) - 1]
d[, df_interaction := df_condition * df_block]
d[, df_within := (.N - 1) - df_condition - df_block - df_interaction]


## NOTE: General linear model (GLM) --- applied to one-way ANOVA:

## xobserved_{ij} = xij --- from one-way table above
## xpredicted_{ij} = mu + tau_{i} + epsilonsq_{ij} --- this is the "model"
## -- where mu is a constant that models non-specific global offset, and
##    epsilonsq ~ N(0, sigma_eps) is a random variable that models non-specific
##    noise within individual subjects.
d <- fread('../data/rbbsdelay/rbbsdelay.csv')
fm <- lm(acc_mean ~ condition, data=d)
anova(fm)
summary(fm)


## NOTE: General lienar model (GLM) --- applied to two-way ANOVA:

## xobserved_{ijk} = xijk --- from two-way table above
## xpredicted_{ijk} = mu + alpha_{i} + beta_{j} + alphabeta_{ij} + epsilonsq_{ijk}
d <- fread('../data/rbbsdelay/rbbsdelay.csv')
fm <- lm(acc_mean ~ condition*block, data=d)
anova(fm)
summary(fm)

## xobserved_{ijk} = xijk --- from two-way table above
## xpredicted_{ijk} = mu + alpha_{i} + beta_{j} + alphabeta_{ij} + epsilonsq_{ijk}
d <- fread('../data/rbbsdelay/rbbsdelay.csv')
fm <- lm(acc_mean ~ condition*block, data=d)
anova(fm)
summary(fm)

## xobserved_{ijk} = xijk --- from two-way table above
## xpredicted_{ijk} = mu + alpha_{i} + beta_{j} + epsilonsq_{ijk}
d <- fread('../data/rbbsdelay/rbbsdelay.csv')
fm <- lm(acc_mean ~ condition + block, data=d)
anova(fm)
summary(fm)





## NOTE: Simple linear Regression compared to one-way ANOVA
## | Simple Linear Regression                         | One-Way ANOVA                                        |
## |--------------------------------------------------+------------------------------------------------------|
## | y_{i} = beta_{0} + beta_{1}*x_{i} + epsilson_{i} | y_{ij} = mu + tau_{i} + epsilson_{i}                 |
## | y_{i} are continuous observed values             | y_{i} are continuous observed values                 |
## | beta_{0} -- y-intercept -- no-treatment mean     | mu -- grand mean -- no-treatment mean                |
## | beta_{1} is a continuous value in (0,1)          | tau_{i} is a discrete value and is called a "factor" |
## | x_{i} are continuous predictor values            | tau_{i} captures beta_{1}*x_{i}                      |
## | epsilon_{i} ~ N(0, sigma_epsilon)                | epsilon_{i} ~ N(0, sigma_epsilon)                    |
## |--------------------------------------------------+------------------------------------------------------|


d <- fread('../data/rbbsdelay/rbbsdelay.csv')

## One-way ANOVA
fm <- lm(acc_mean ~ condition, data=d)
anova(fm)
summary(fm)
## TODO: Plot mean acc per condition

## Simple linear regression
x <- d[block==16, acc_mean]
y <- d[block==17, acc_mean]
fm <- lm(y ~ x)
anova(fm)
summary(fm)
## TODO: plot x vs y with regression line


## NOTE: Relationship between model parameters, F-score, p-value, etc.
## The model and output as reference
m1 <- lm(dist ~ speed, data = cars)
summary(m1)
summary.aov(m1) # To get the sums of squares and mean squares

## Calculate sums of squares (total, residual and model)
y <- cars$dist
ybar <- mean(y)

## total sums of squares (SStotal)
## - SStotal assess how well the mean fits the data.
ss.total <- sum((y-ybar)^2)

ggplot(cars, aes(x=speed, y=dist)) +
  geom_point() +
  geom_hline(yintercept = ybar) +
  geom_segment(aes(xend=speed, y=ybar, yend=dist), colour="red") +
  theme(aspect.ratio = 1)

## residual sums of squares (SSresidual)
## - SSResidual assess how well the regression line fits the data.
ss.residual <- sum((y-m1$fitted)^2)

ggplot(cars, aes(x=speed, y=dist)) +
  geom_point() +
  geom_abline(slope = m1$coefficients[2], intercept = m1$coefficients[1]) +
  geom_segment(aes(xend=speed, y=m1$fitted.values, yend=dist), colour="red") +
  theme(aspect.ratio = 1)

## model sums of squares (SSmodel)
## - SSmodel compares how much better the regression line is compared to the
##   mean (i.e. the difference between the SStotal and the SSresidual)
ss.model <- ss.total-ss.residual

ggplot(cars, aes(x=speed, y=dist)) +
  geom_hline(yintercept = ybar) +
  geom_abline(slope = m1$coefficients[2], intercept = m1$coefficients[1]) +
  geom_segment(aes(xend=speed, y=m1$fitted.values, yend=ybar), colour="red") +
  theme(aspect.ratio = 1)

## Calculate degrees of freedom (total, residual and model)
n <- length(cars$speed)
k <- length(m1$coef) # k = model parameter: b0, b1
df.total <- n-1
df.residual <- n-k
df.model <- k-1

## Calculate mean squares (note that these are just variances)
ms.residual <- ss.residual/df.residual
ms.model<- ss.model/df.model

## Calculate residual standard error
res.se <- sqrt(ms.residual)

## R squared
## The R2 expresses how much of the total variation in the data can be explained
## by the model (the regression line).
## the difference between the RSE and the R2 is that the RSE tells you something
## about the inaccuracy of the model (in this case the regression line) given the
## observed data. The R2 on the other hand tells you how much variation is
## explained by the model (i.e. the regression line) relative the variation that
## was explained by the mean alone (i.e. the simplest model).
r.sq <- ss.model/ss.total

## Calculate F-value
F <- ms.model/ms.residual

## Calculate P-value
## F-value expresses how much of the model has improved (compared to the mean)
## given the inaccuracy of the model
p.F <- pf(F, df.model, df.residual)


## NOTE: Method of least squares: How are best fitting parameters found?
## Anwer: The best fitting parameters are those that minimize:
## sum((y_predicted - y_observed)^2)


## TODO: repeated measures ANOVA
## TODO: confidence intervals
