library('data.table')
library('ggplot2')
library('gtools')

rm(list=ls())

## NOTE: Problem 0:
## Between 9:00 AM and 9:30 AM one day, a garage mechanic will check the headlight alignment of two cars. For each car, the result will be recorded as follows:
## O = both lights are in alignment
## L = only the left light is out of alignment
## R = only the right light is out of alignment
## LR = Both lights are out of alignment

## (a) List the sample space

## (b) Give the composition of the following events:
## A = the first car has both lights out of alignment
## B = the left light is out of alignment in both cars
## C = Exactly one of the cars has both lights in alignment

## (c) Give the composition of the following events:
## A U B
## AB
## compliment of (A U B)


## NOTE: Problem 1:
## The following table lists the number of survivors in each age group for a
## cohort of 1000 white male births in the U.S.
## E.g., 798 survived at least to their 10th birthday

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

## (b) compute the relative frequencies of each event (i.e., the different rows
## of the Age column). For the remainder of this exervise, use these frequencies
## as estimates for the true probabilities (which we do not know).

## (c) For a new birth, what is probability of survival beyond age 40?

## (d) For a new birth, what is the probability of death between the ages of 40 and 60?


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

## (b) Find the probabilities P(A), P(B), P(B & C), P(A & B & C)

## (c) P(serious | He is below 40)

## (d) P(C | A)

## (e) P(compliment of A | B)


## NOTE: Problem 3:
## Toss a pair of dice 50 times and record the sum of the numbers that face up
## each time.

## (a) Find the relative frequency of event A = {Sum is 8 or more}

## (b) Gradually increase the sample size (i.e., the number of times we roll the
## die) and plot the relative frequency of events A (from above) as a function
## of the number of rolls. Note that if the die are balanced, the probability of
## A is 15/36. Say something smart about this graph.


## NOTE: Problem 4
## A forest contains 25 elks of a rare species, 5 of which are captured, tagged,
## and released. After some time, the forester will capture 6 elks and count the
## number of tagged animals. Assume that no elks migrated in or out of the
## forest between the first and second captures.

## (a) Assuming that the elk are all captured at once, what is the probability
## that 2 of the 6 elks captured on the second occasion will be tagged? Answer
## this question by building a suitable data.table and counting rows.

## (b) Repeat (a) but use R's built in n choose k function

## (c) Assuming that the elk are captured sequentially, what is the probability
## that the first 2 of the 6 elks captured on the second occasion will be
## tagged? Answer this question by building a suitable data.table and counting
## rows.

## (d) Repeat (c) but use permutation equations

## (e) Repeat (c) but use conditional probability
