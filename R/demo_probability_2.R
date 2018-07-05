library('data.table')
library('ggplot2')

## we will use an additional package today
install.packages('gtools')
library(gtools)

rm(list = ls())


## NOTE: Suppose that on a 4 question true / false exam, a student decides to
## respond randomly.

## (a) list the sample space using:
## R = correct guess
## W = incorrect guess

## By hand we can come up with:

## TODO: Go over a systematic way of getting sample space in this sort of
## situation. Start with N=1 and work up to N=4.

## RRRR
## RRRW
## RRWR
## RRWW
## RWRR
## RWRW
## RWWR
## RWWW
## WRRR
## WRRW
## WRWR
## WRWW
## WWRR
## WWRW
## WWWR
## WWWW

e <- c('R', 'W')
s <- permutations(n=2, r=4, v=e, repeats.allowed=T)
d <- as.data.table(s)

## (b) what is the probability that the student will guess correctly at least
## one time?

## We just need to count the number of possible events that have at least 1
## correct response (and realize that an event is just a row in out data.table).

## by hand we see that it's:
15/16

## Using data.table we see:
d[V1=='R' | V2=='R' | V3=='R' | V4=='R', .N] / d[, .N]

## (c) what is the probability that the student will guess correctly at least 2
## times?

## By hand we can see that it's:
11/16

## Using data.table we see:
dd <- as.data.table(d[, .SD == 'R'])
dd[, i := 1:.N]
dd[, sum(c(V1, V2, V3, V4)) >= 2, .(i)][, sum(V1)] / d[, .N]

## (d) what is the probability that the student will guess correctly at least 3
## times?

## By hand we can see that it's:
5/16

## Using data.table we see:
dd <- as.data.table(d[, .SD == 'R'])
dd[, i := 1:.N]
dd[, sum(c(V1, V2, V3, V4)) >= 3, .(i)][, sum(V1)] / d[, .N]

## (e) what is the probability that the student will guess correctly 3 or 4
## times?

## By hand we can see that it's:
5/16

## By being clever we know that it's:
4/16 + 1/16

## Using data.table we see:
dd <- as.data.table(d[, .SD == 'R'])
dd[, i := 1:.N]
dd[, s := sum(c(V1, V2, V3, V4)), .(i)]
dd[s==3 | s==4, .N] / d[, .N]


## NOTE:
## When an experimental stimulus is given to an animal, it either responds (R)
## of fails to respond (F). Consider an experiment that consists of
## administering the stimulus to three animals in succession and recording R or
## N for each animal.

## (a) List the sample space for this experiment

## We know how to do this by hand now, and it's a pretty small sample space, so
## give it a whirl...
## e1 = RRR
## e2 = RRF
## e3 = RFR
## e4 = FRR
## e5 = RFF
## e6 = FRF
## e7 = FFR
## e8 = FFF

## R of course does this for you if you like
e <- c('R', 'F')
s <- permutations(n=2, r=3, v=e, repeats.allowed=T)
d <- as.data.table(s)

## (b) List the compositions of the following events:

## (i) A = only one animal responds

## By hand:
## A = {e3, e4, e5}

## Using data.table
dd <- as.data.table(d[, .SD == 'R'])
dd[, i := 1:.N]
dd[, s := sum(c(V1, V2, V3)), .(i)]
dd[s == 1]

## (ii) B = there is a response on the first trial

## By hand:
## B = {e1, e2, e3, e5}

## Using data.table
d[V1=='R']

## (iii) C = both the first and the second animals fail to respond

## By hand:
## C = {e7, e8}

## Using data.table:
d[V1=='F' & V2=='F']

## (iv) A U B

## By hand:
## {e1, e2, e3, e5, e6, e7}

## Using data.table:
dd[s == 1 | V1==TRUE]

## (v) AC

## By hand:
## {e7}

## Using data.table:
dd[s==1][V1==FALSE & V2==FALSE]

## (vi) BC

## By hand:
## {}

## Using data.table:
dd[V1==TRUE][V1==FALSE & V2==FALSE]

## (vii) compliment of B

## By hand:
## {e4, e6, e7, e8}

## Using data.table:
dd[!(V1==TRUE)]

## (viii) compliment of (A U B)

## By hand:
## {e4, e8}

## Using data.table
dd[!(s == 1 | V1==TRUE)]


## NOTE:
## The following table shows the proportion of students falling into performance
## categories in math and physical education.

## | Math    | Physical Education | Proportion of students |
## |---------+--------------------+------------------------|
## | Good    | Good               |                    .06 |
## | Good    | Average            |                    .11 |
## | Good    | Poor               |                    .18 |
## | Average | Good               |                    .12 |
## | Average | Average            |                    .18 |
## | Average | Poor               |                    .10 |
## | Poor    | Good               |                    .16 |
## | Poor    | Average            |                    .05 |
## | Poor    | Poor               |                    .04 |

## Let,
## A = student is good in math
## B = student is good in PE

## (a) What is P(A U B)?

## You might think to just count up all the rows for which there is a Good.
## P(A) = .06 + .11 + .18
## P(B) = .06 + .12 + .16

## Then, P(A U B) = P(A) + P(B) (WRONG)

## But this counts the (Good, Good) row twice, which isn't correct. This is why
## the general addition law of probability is:

## P(A U B) = P(A) + P(B) - P(AB)
## P(A U B) = (.06 + .11 + .18) + (.06 + .12 + .16) - .06


## NOTE:
## There are 4 people competing in a bicycle race.

## (a) In how many ways can the 1st and 2nd prizes be awarded?
## ab
## ac
## ad

## ba
## bc
## bd

## ca
## cb
## cd

## da
## db
## dc

## (b) In how many ways can the 1st, 2nd, and 3rd prizes be awarded?
## abc
## abd
## acb
## acd
## adb
## adc

## bac
## bad
## bca
## bcd
## bda
## bdc

## cab
## cad
## cba
## cbd
## cda
## cdb

## dab
## dac
## dba
## dbc
## dca
## dcb

## (c) repeat (a) and (b) for 5 people competing instead of just 4...

## NOPE! No way I'm doing this the long way.
## We can take the approach we've seen so far by using permutations() to list
## the sample space.

## First versify by reproducing what we've already done by hand
e <- c('a', 'b', 'c', 'd')
s <- permutations(n=length(e), r=3, v=e, repeats.allowed=F) # Note the F
d <- as.data.table(s)

## Now do it for the current problem
e <- c('a', 'b', 'c', 'd', 'e')
s <- permutations(n=length(e), r=3, v=e, repeats.allowed=F)
d <- as.data.table(s)

## Turns out there is a simple way to express this mathematically.
## The number of ways to arrange n objects is given by:
## n! = (n) * (n-1) * (n-2) * ... * (2) * (1)

## The number of ways to arrange r objects selected from n distinct objects is:
## n * (n-1) * (n-2) * ... * (n-r+1)

## which is equal to the first r terms of n! and can be expressed:
## n! / (n-r)!
factorial(5) / factorial(5-3)


## NOTE:
## Enumerate all the possible (a) combinations and (b) arrangements of three
## letters chosen from the four letters A, B, C, and D:

## (a) combinations:

## This is simple enough to do by hand:
## {A, B, C}
## {A, B, D}
## {A, C, D}
## {B, C, D}

## Using R (gtools package):
e <- c('A','B','C','D')
combinations(length(e), 3, e, repeats.allowed = F)

## (b) arrangements:
## {ABC, ACB, BAC, BCA, CAB, CBA}
## {ABD, ADB, DAB, DBA, BAD, DBA}
## {ACD, ADC, CAD, CDA, DAC, DCA}
## {BCD, BDC, CBD, CDB, DBC, DCB}
e <- c('A','B','C','D')
permutations(length(e), 3, e, repeats.allowed = F)


## NOTE:
## Consider an urn containing 12 articles of which 8 are good (G1, G2,...,G8)
## and 4 are defective (D1, D2, D3, D4). Suppose that 3 articles will be drawn
## from the urn simultaneously.

## (a) How many distinct results are possible

## Do it the very long ways
## Begin by defining the elementary outcomes (the articles in the urn)
e <- c('G1',
       'G2',
       'G3',
       'G4',
       'G5',
       'G6',
       'G7',
       'G8',
       'D1',
       'D2',
       'D3',
       'D4')

## Order doesn't matter so we use "combinations"
## Once we sample an article, it is out of the urn, so we sample without
## replacement
s <- combinations(n=length(e), r=3, v=e, repeats.allowed=F)

## Do it the long way:
## (n!/(n-r)!) / r!
(factorial(12) / factorial(12-3)) / factorial(3)

## Do it using choose()
choose(12, 3)

## (b) How many results are possible for which two of the items are defective
## and one is good?

## We use the product rule:
## When an experiment consists of two parts such that the first part can have k
## distinct results, and if with each result of the first part there can be l
## distinct results of the second part, then the total number of possible
## results of the experiment is k * l

## Number of ways to get two defective items
n_defective <- choose(4, 2)

## Number of ways to get one good items
n_good <- choose(8,1)

## So the total number of ways to get 2 defective and one good is:
n <- n_defective * n_good

## (c) What is the probability of obtaining two defective pieces and one good
## piece (assuming you select randomly)?

## Compute the total number of possible outcomes
N <- choose(12,3)

## p is just the usual proportion
p <- n/N


## NOTE:
## A group of executives are classified according to the status of body weight
## and incidence of hypertension. The following table lists proportions of these
## folks.

## |                  | overweight | normal weight | underweight | total |
## |------------------+------------+---------------+-------------+-------|
## | hypertensive     |        .10 |           .08 |         .02 |   .20 |
## | not hypertensive |        .15 |           .45 |         .20 |   .80 |
## |------------------+------------+---------------+-------------+-------|
## | total            |        .25 |           .53 |         .22 |  1.00 |

## (a) What is the probability that a person selected at random from this group
## will have hypertension?

## Here, we don't care about weight, so we sum over all possible weights
## P(hypertensive) = .20

## (b) What is the probability that a person, selected at random from this
## group, who is found to be overweight, will also have hypertension?

## Here, we know the person is overweight, so we focus on that column.
## P(hypertensive | overweight) = .10 / .25
## This is read "the conditional probability of hypertension given
## overweight..."


## NOTE:
## farmer has a box containing 30 eggs, 5 of which have blood spots. He checks 3
## eggs at random one after the other. What is the probability that the first
## two eggs will have spots and the third will be clear?

## Start with some foundation work:
## S = spotted
## C = clear

## Can we do this by just listing the sample space and counting by hand?
## Absolutely, 30 eggs is a lot to list and count, so if we take this approach,
## then definitely use the R tools we have been learning.

## (b) Do this via conditional probability
## P(S1 S2 C3) = P(S1) * P(S2|S1) * P(C3|S1 S2)

## On the first sample, there are 5 spotted out of 30 total eggs, and since we
## sample at random:
p_s1 <- 5/30

## on the segond sample, there are 4 spotted out of a remaining 29 eggs
p_s2gs1 <- 4/29

## on the third draw, there are 25 clear eggs out of a remaining 28 eggs
p_c3gs1s2 <- 5/28

## all that is left is to multiply them all together
## Since my variable names kinda suck, I'm just gonna type the number in...
p <- (5/30) * (4/29) * (25/28)


## (b) Do this by counting:

## Remember that we care about order here
## The number of possible ordered outcomes is:
N <- factorial(30) / factorial(30-3)

## The number of ordered ways to get two spotted
n_2_spotted <- factorial(5) / factorial(5-2)

## The number of ordered ways to get 1 clear
n_1_clear <- factorial(25) / factorial(25-1)

## using the product rule for probabilities we get:
p <- (n_2_spotted * n_1_clear) / N


## (c) What is the probability of two spotted eggs and 1 clear egg in any order?

## The total number of possible outcomes is:
N <- choose(30,3)

## The number of ways to select two spotted eggs is:
n_2_spotted <- choose(5, 2)

## The number of ways to select one clear egg is:
n_1_clear <- choose(25,1)

## using the product rule for probabilities we get:
p <- (n_2_spotted * n_1_clear) / N


## NOTE: Consider the experiment of tossing a fair coin two times. The symmetry
## of the experiment indicates that the outcomes {HH, HT, TH TT} are equally
## likely. Define the following three events:

## A = H on the first toss
## B = H on the second toss
## C = HH or TT

## Then, assuming the coin is fair:
## P(H) = .5
## P(T) = .5

## P(HH) = .25
## P(HT) = .25
## P(TH) = .25
## P(TT) = .25

## P(A) = .5
## P(B) = .5
## P(C) = .25 + .25 = .5

## (a) are A and B independent?
## P(A) * P(B) = .5 * .5 = .25
## P(AB) = P(HH) = .25
## P(A) * P(B) == P(AB) so A and B are independent

## (b) are B and C independent?
## P(B) * P(C) = .5 * .5 = .25
## P(BC) = P(HH) = .25
## P(B) * P(C) == P(BC) so B and C are independent

## (c) are A and C independent?
## P(A) * P(C) = .5 * .5 = .25
## P(AC) = P(HH) = .25
## P(A) * P(C) == P(AC) so A and C are independent


## NOTE:
## A group of executives are classified according to the status of body weight
## and incidence of hypertension.

## |                  | overweight | normal weight | underweight | total |
## |------------------+------------+---------------+-------------+-------|
## | hypertensive     |        .10 |           .08 |         .02 |   .20 |
## | not hypertensive |        .15 |           .45 |         .20 |   .80 |
## |------------------+------------+---------------+-------------+-------|
## | total            |        .25 |           .53 |         .22 |  1.00 |

## Define two events:
## A = hypertensive
## B = overweight

## (a) Are A and B independent?
## P(hypertensive) = .20
## P(overweight) = .25
## P(hypertensive and overweight) = .10
## P(hypertensive) * p(overweight) == P(hypertensive and overweight)
.20 * .25 == .10

## Nope. A and B are not independent.


## NOTE:
## A blood disease is present in about 2% of the population in a serious form,
## in 10% in a light form, and not present at all in the remaining 88%. A
## clinical test is 90% successful in detecting the disease in serious cases. It
## is successful 60% of the time for light cases. It will yield a false positive
## in the healthy population in 10% of cases. A person selected at random from
## the population is given the test, and the result is positive.

## S = serious case
## L = light case
## N = no disease
## + = test positive
## - = test negative

## The following probabilities are given in the problem:
## P(S) = .02
## P(L) = .10
## p(N) = .88
## P(+|S) = .9
## P(+|L) = .6
## P(+|N) = .1

## What is the probability that this person has a serious case of the disease?

## We are really being asked for:
## P(S|+) = P(S,+) / P(+)

## We are not explicitly given P(S,+), but we can compute it.
## P(S,+) = P(S) * P(+|S)
p_s_plus <- .02 * .9

## We are also not explicitly given P(+), but we can compute it
## P(+) = p(S+) + P(L+) + P(N+)

## So we need to compute a few things, it would seem...
## P(P,+) = P(P) * P(+|P)
p_l_plus <- .10 * .6

## P(N,+) = P(N) * P(+|N)
p_n_plus <- .88 * .1

## so the probability that this person has a serious case of the disease is:
## P(S|+) = P(S,+) / P(+)
p_s_given_plus = p_splus / p_plus

