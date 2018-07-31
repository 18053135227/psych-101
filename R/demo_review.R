library('data.table')
library('ggplot2')

################################################################################
################################################################################

## NOTE: Review and consolidation of NHST

## First, perform an experiment / gather some data.
## Then let,
## X = The RV that generates data for your experiment
## x = a sample from the RV X

## Step 1: state hypotheses

## Step 2: choose confidence

## Step 3: specify the distribution of a statistic that estimates the population
##         parameter in step 1 and compute it's value from your x observations
##         (for now, call the result x_H_obs).
## X ~ how do you think X is distributed?
## X_H_obs = what statistic will you use to estimate the parameters in H0?
## X_H_obs ~ how do you think X_H_obs is distributed?

## Step 4: Determine rejection criteria.
##         - compute a p-value: P(x_H_obs | H0 is true)
##         - compute critical values: x_H_crit = the most extreme value of
##         - x_H_obs that we will tolerate before we would decide to reject H0.

## Step 5: Make a decision.

################################################################################
################################################################################

## NOTE: If the class is a representative sample of all students at UC Berkeley,
## then test the hypothesis that the probability of a UCB student is wearing
## shoes with laces is greater than chance.

## (a) what type of test will we do?
## (b) do the test long hand / code
## (c) do the test short hand / code


## NOTE: If the class is a representative sample of all students at UC Berkeley,
## then test the hypothesis that the average height of all students at UC
## Berkeley is greater than 5'5".

## (a) what type of test will we do?
## (b) do the test long hand / code
## (c) do the test short hand / code


## NOTE: If the class is a representative sample of all students at UC Berkeley,
## then test the hypothesis that students that more students prefer to sit on
## the left side of the room than the right.

## (a) what type of test will we do?
## (b) do the test long hand / code
## (c) do the test short hand / code


## NOTE: If the class is a representative sample of all students at UC Berkeley,
## then test the hypothesis that the number of units a student is enrolled in
## this summer is less then the number of units they were enrolled in during the
## previous academic session.

## (a) what type of test will we do?
## (b) do the test long hand / code
## (c) do the test short hand / code


## NOTE: If the class is a representative sample of all students at UC Berkeley,
## then test the hypothesis that the variance in height between all students at
## UC Berkeley is greater than 10

## (a) what type of test will we do?
## (b) do the test long hand / code
## (c) do the test short hand / code


## NOTE: If the class is a representative sample of all students at UC Berkeley,
## then test the hypothesis that the variance in height is greater in students
## that sit on the left side of class versus students that sit on the right side
## of class.

## (a) what type of test will we do?
## (b) do the test long hand / code
## (c) do the test short hand / code


## NOTE: ANOVA...
