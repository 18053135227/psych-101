library('data.table')
library('ggplot2')

rm(list = ls())

## NOTE: Suppose that on a N question true / false exam, a student decides to
## respond randomly.

## (a) List the sample space for when N = 5
## (b) List the sample space for when N = 6

## NOTE: The sample space for the response of a single person's attitude toward
## a political issue consists of the three elementary outcomes:

## e1 = {unfavorable}
## e2 = {favorable}
## e3 = {undecided}

## Are the following assignments of probability permissible?
## (a) p(e1) = .4, p(e2) = .5, p(e3) = .1
## (b) p(e1) = .4, p(e2) = .4, p(e3) = .4
## (c) p(e1) = .5, p(e2) = .5, p(e3) = 0

## NOTE:
## Construct a sample space for each of the following experiments:

## (a) Someone claims to be able to taste the difference between the same brand
## of bottled, tap, and canned draft beer. A glass of each is poured and given
## to the subject in an unknown order. The subject is asked to identify the
## contents of each glass. The number of correct identifications will be
## recorded.

## (b) The number of traffic fatalities in a state next year.

## (c) The length of time a new TV will continue to work satisfactorily without
## service.

## NOTE:
## Between 9:00 AM and 9:30 AM one day, a garage mechanic will check the
## headlight alignment of two cars. For each car, the result will be recorded as
## follows:
## Q = Both lights are in alignment
## L = Only the left light is out of alignment
## R = Only the right light is out of alignment
## LR = Both lights are out of alignment

## (a) Using this notation, list the sample space of the experiment.

## (b) Give the compositions of the following events:
## (i) A = The first car has both lights out of alignment
## (ii) B = The let light is out of alignment in both cars
## (iii) C = Exactly one of the cars has both lights in alignment

## (c) Give the compositions of the events:
## (i) A U B
## (ii) the compliment of A U B
## (iii) AB

## NOTE:
## A sample space has 8 elementary outcomes {e1, e2, ... , e8}.
## Suppose the events A, B, and C have the following compositions:

## A = {e1, e2, e3, e4}
## B = {e2, e4, e5, e6}
## C = {e3, e4, e6, e7}

## Given:

## P(A) = .4
## P(B) = .5
## P(C) = .5
## p(AB) = .2
## P(AC) = .2
## P(BC) = .2
## P(ABC) = .1

## (a) What are the probabilities of each elementary outcome?


## (b) Give the compositions for the following events and find their
## probabilities:
## (i) A U B
## (ii) the compliment of A U B U C
## (iii) The compliment of AB

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

## (b) compute the relative frequencies of each event (i.e., the different rows
## of the Age column). For the remainder of this exervise, use these frequencies
## as estimates for the true probabilities (which we do not know).

## (c) For a new birth, what is probability of survival beyond age 40?

## (d) For a new birth, what is the probability of death between the ages of 40 and 60?
## We are asked for: P(40 < death < 60)
## P(40 < death < 60) = P(survive to 40) - P(survive to 60)
## Note that bin 40-49 is index 5, and bin 60-69 is index 7
## Here are two simple ways to compute this

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
## We will estimate probability with relative frequency
## P(A) = P(serious case)

## P(B) = P(He is below 40)

## P(B & C) = P(He is below 40 AND his parents are diabetic)

## P(A & B & C)

## NOTE: Problem 3:
## Toss a pair of dice 50 times and record the sum of the numbers that face up
## each time.
## (a) Find the relative frequency of event A = {Sum is 8 or more}

## define the dice sample space

## define the probability of each outcome (assume balanced dice)

## roll the dice and record the results

## put results in a data.table

## Find the requested relative frequency
## We're using data.table "chaining " here.

## (b) Gradually increase the sample size (i.e., the number of times we roll the
## die) and plot the relative frequency of events A (from above) as a function
## of the number of rolls. Note that if the die are balanced, the probability of
## A is 15/36. Say something smart about this graph.

