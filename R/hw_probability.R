library('data.table')
library('ggplot2')

## NOTE: Problem 1:
## The following table lists the number of survivors in each age group for a
## cohort of 1000 white male births in the U.S.
## E.g., 708 survived at least to their 10th birthday

## |         Age | Number of Survivors |
## |-------------+---------------------|
## |         0-9 |                1000 |
## |       10-19 |                 798 |
## |       20-29 |                 772 |
## |       30-39 |                 743 |
## |       40-49 |                 712 |
## |       50-59 |                 665 |
## |       60-69 |                 573 |
## | 70 and over |                 407 |
## |-------------+---------------------|

## (a) Build a data.table representation of the survival table
age <- 1:8
n_surv <- c(1000, 798, 772, 743, 712, 665, 573, 407)
d <- data.table(age, n_surv)

## (b) compute the relative frequencies of each event (i.e., the different rows
## of the Age column). For the remainder of this exervise, use these frequencies
## as estimates for the true probabilities (which we do not know).
d[, rf := n_surv / 1000]

## (c) For a new birth, what is probability of survival beyond age 40?
d[age == 4, sum(rf)]

## (d) For a new birth, what is the probability of death between the ages of 40 and 60?
## We are asked for: P(40 < death < 60)
## P(40 < death < 60) = P(survive to 40) - P(survive to 60)
## Note that bin 40-49 is index 5, and bin 60-69 is index 7
## Here are two simple ways to compute this
d[age==5, rf] - d[age==7, rf]
d[, rf[age==5] - rf[age==7]]

## NOTE: Problem 2:
## The medical records of the male diabetic patients reporting to a clinic
## during one year provide the following percentages:

## |                | Light Case          | Light Case          | Serious Case        | Serious Case        |
## |                | Diabetes in parents | Diabetes in parents | Diabetes in parents | Diabetes in parents |
## |                | Yes                 | No                  | Yes                 | No                  |
## | Age of Patient |                     |                     |                     |                     |
## |----------------+---------------------+---------------------+---------------------+---------------------|
## | Below 40       | 15                  | 10                  | 8                   | 2                   |
## | Above 40       | 15                  | 20                  | 20                  | 10                  |

## Suppose a patient is chosen at random from this group, and the events A, B,
## and C are defiend.
## A: He has a serious case
## B: He is below 40
## C: His parents are diabetic

## (a) Build a data.table representation of the data
case = rep(c('light', 'serious'), 2, each=2)
parents = rep(c('yes','no'), 4)
age = rep(c('above', 'below'), each=4)
N = c(15, 10, 8, 2, 15, 20, 20, 10)
d <- data.table(case,parents,age,N)

## (b) Find the probabilities P(A), P(B), P(B & C), P(A & B & C)
## We will estimate probability with relative frequency
## P(A) = P(serious case)
d[, sum(N[case=='serious']) / sum(N)]

## P(B) = P(He is below 40)
d[, sum(N[age=='below'] / sum(N))]

## P(B & C) = P(He is below 40 AND his parents are diabetic)
d[, sum(N[age=='below' & parents=='yes'] / sum(N))]

## P(A & B & C)
d[, sum(N[case=='serious' & age=='below' & parents=='yes'] / sum(N))]

## NOTE: Problem 3:
## Toss a pair of dice 50 times and record the sum of the numbers that face up
## each time.
## (a) Find the relative frequency of event A = {Sum is 8 or more}

## define the dice sample space
die_1 <- 1:6
die_2 <- 1:6

## define the probability of each outcome (assume balanced dice)
p_die_1 <- rep(1,6)
p_die_2 <- rep(1,6)

## roll the dice and record the results
n_rolls <- 50
roll_1 <- sample(die_1, n_rolls, replace=TRUE, prob=p_die_1)
roll_2 <- sample(die_2, n_rolls, replace=TRUE, prob=p_die_2)

## put results in a data.table
d <- data.table(roll_1, roll_2, roll=1:n_rolls)

## Find the requested relative frequency
## We're using data.table "chaining " here.
d[, sum(roll_1, roll_2) >= 8, .(roll)][, sum(V1) / n_rolls]

## (b) Gradually increase the sample size (i.e., the number of times we roll the
## die) and plot the relative frequency of events A (from above) as a function
## of the number of rolls. Note that if the die are balanced, the probability of
## A is 15/36. Say something smart about this graph.
n_rolls <- seq(10,500,5)
n_exps <- length(n_rolls)
exp_number <- c()
exp_results_1<- c()
exp_results_2<- c()
for(i in 1:n_exps) {
  exp_number <- c(exp_number, rep(i, n_rolls[i]))
  exp_results_1 <- c(exp_results_1,
                     sample(die_1, n_rolls[i], replace=TRUE, prob=p_die_1))
  exp_results_2 <- c(exp_results_2,
                     sample(die_2, n_rolls[i], replace=TRUE, prob=p_die_2))
}

d <- data.table(exp_number, exp_results_1, exp_results_2)

## TODO: Pick up here by converting the line below to work
## d[, sum(roll_1, roll_2) >= 8, .(roll)][, sum(V1) / n_rolls]

## ggplot(d[exp_results==6], aes(x=n_rolls, y=p_est)) +
##   geom_point() +
##   geom_hline(yintercept=p2, colour='red') +
##   ylim(0,.5)

