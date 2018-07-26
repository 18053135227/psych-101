library('data.table')
library('ggplot2')

rm(list = ls())


## NOTE: One-way ANOVA
## Suppose ou have k different treatment groups. A one-way ANOVA asks if there
## are any differences in the effects of treatment between any of the goups.
## This type of test is called an "omnibus" test.

## The hypotheses look like this:
## H0: mu1 = mu2 = mu3 ...
## H1: not H1


## NOTE: Intuitive explanation for how a one-way ANOVA works
## between-group variation --- how different are group means from each other?
## within-group variation --- how noisy is your data in general?

## If means are very different, and noise is very small, then reject H0
## Else, don't reject H0


## NOTE: Another blurb for intuition:
## https://stats.stackexchange.com/questions/40549/how-can-i-explain-the-intuition-behind-anova
## ANOVA is statistical technique used to determine whether a particular
## classification of the data is useful in understanding the variation of an
## outcome. Think about dividing people into buckets or classes based on some
## criteria, like suburban and urban residence. The total variation in the
## dependent variable (the outcome you care about, like responsiveness to an
## advertising campaign) can be decomposed into the variation between classes and
## the variation within classes. When the within-class variation is small relative
## to the between-class variation, your classification scheme is in some sense
## meaningful or useful for understanding the world. Members of each cluster behave
## similarly to one another, but people from different clusters behave
## distinctively. This decomposition is used to create a formal F test of this
## hypothesis.


## NOTE: Assumptions of ANOVA
## - The observations are obtained independently and randomly from the population
##   defined by the factor levels
## - The data of each factor level are normally distributed.
## - These normal populations have a common variance.


## NOTE: Try out an example
d <- as.data.table(PlantGrowth)

ggplot(d, aes(x=group, y=weight)) +
  geom_boxplot() +
  theme(aspect.ratio = 1)

# Step 0: Calculate the number of groups and observations per group
d[, n_groups := length(unique(group))]
d[, n_obs := .N, .(group)]


## Step 1: Calculate the mean within each group:
d[, weight_mean_group := mean(weight), .(group)]


## Step 2: Calculate the overall mean:
d[, weight_mean_grand := mean(weight)]


## Step 3: Calculate the between-group sum of squared differences:
d[, ss_between := sum((weight_mean_group - weight_mean_grand)^2)]


## Step 4: Calculate the "within-group" sum of squared differences.
d[, ss_within := sum((weight - weight_mean_group)^2)]


## Step 5: Compute degrees of freedom
d[, df_between := n_groups-1]
d[, df_within := n_groups*(n_obs-1)]


## Step 6: Calculate MSE terms
d[, mse_between := ss_between / df_between]
d[, mse_within := ss_within / df_within]


## Step 7: Calculate the F-ratio
d[, F := (ss_between/df_between) / (ss_within/df_within)]


## NOTE: Or... do it with built-in R functions
fm <- lm(weight ~ group, data = d)
anova(fm)


## NOTE: Discussion on types of SSEs
## https://www.r-bloggers.com/anova-%E2%80%93-type-iiiiii-ss-explained/
## Take-home message: When the data are balanced, don't worry.
## If the data are unbalanced, then go with this:
options(contrasts = c(“contr.sum”,”contr.poly”))


## TODO: If time permits, then do some real data examples of each of the F-tests
## we just looked at. This will be real-time coding... which might be cool for
## the class to see.

