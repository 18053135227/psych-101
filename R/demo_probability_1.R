library('data.table')
library('ggplot2')

rm(list = ls())

## Goal: Uncerstand some basic concepts of probability through the magic of R

## TODO:
## (i) Motivate this lecture by starting with a broad overview of NHST

## (ii) Admit that we are still not quite to NHST / inferential stats... we are
##      still building the necessary tools to do so.

## (iii) Important definitions and high level concepts:


## NOTE: sample space: the collection of all possible distinct outcomes of an
## experiment

## discrete sample space: possible outcomes are finite or countably infinite
## continuous sample space: possible outcomes include all the numbers in some
## interval

## TODO: List the sample space for each of the following experiments
## Exp. A: Note the sex of the first two newborns in town tomorrow.

## Exp. B: Let each of 10 persons taste a cup of instant coffee and a cup of
## percolated coffee. Record how many people prefer the instant coffee.

## Exp. C: Administer an antibiotic to patients suffering from a viral infection
## until one has an adverse reaction.

## Exp. D: Measure the concentration of dust in the air near a construction
## site.

## Exp. E: Give a child a specific dose of a multivitamin in addition to a
## normal diet. Observe the child's height and weight gains after 12 weeks.

## TODO: Answers
## Exp. A: Note the sex of the first two newborns in town tomorrow.
S <- list(c('B', 'B'),
          c('B', 'G'),
          c('G', 'B'),
          c('G', 'G'))

## Exp. B: Let each of 10 persons taste a cup of instant coffee and a cup of
## percolated coffee. Record how many people prefer the instant coffee.
S <- 1:10

## Exp. C: Administer an antibiotic to patients suffering from a viral infection
## until one has an adverse reaction.
S <- list(c('R'), c('N','R'), c('N', 'N', 'R'))
## Note that this goes on forever... if only there were a different way...

## Exp. D: Measure the concentration of dust in the air near a construction
## site.
## The concentration of dust is just the proportion of dust per unit volume of
## air, so it can be any number between 0 and 1.
S <- 0:1

## Exp. E: Give a child a specific dose of a multivitamin in addition to a
## normal diet. Observe the child's height and weight gains after 12 weeks.
## Both height and weight are measured on a continuous scale.
## height won't get smaller, so it's [0 Inf]
## weight could be anything so it's [-Inf, Inf]
## Hard to concisely state S in this case, but the previous two lines is good
## enough for us for now.

## NOTE: Conditions for a model of probability for discrete sample spaces
## (a) 0 <= P(A) <= 1 for all events A
## (b) For some set of events, B, P(B) is the sum of probabilities for all
##     elementary events e belonging to A.
## (c) The probability of all events in a sample space must sum to 1

## NOTE: Conditions for a model of probability for continuous sample spaces...
## TODO: Since most / all of what we will do today is discrete, it's probably
## safe to put this aside for now.

## TODO: Do this as a class
## Examining validity of probabilities:
## The experiment of observing the sex of the older and the younger children in two-children families has four possible outcomes:

## e1 = (B,B)
## e2 = (B,G)
## e3 = (G,B)
## e4 = (G,G)

## Do each of the following assignments of probability satisfy the conditions of
## a probability model?
## (a) p(e1) = p(e2) = p(e3) = p(e4) = 1/4
## (b) p(e1) = 3/16, p(e2) = 3/8, p(e3) = 1/4, p(e4) =3/16
## (c) p(e1) = 1/2, p(e2) = 1/4, p(e3) = 1/4, p(e4) = 1/2

## Consider the experiment of tossing a balanced die and recording the number on
## the face that shows up. Each of the 6 faces on this symmetric cube is as
## likely to appear as any other. Without performing the experiment, we can
## logically conclude that the proportion of times each particular face is
## expected to occur is 1/6. Any other assignment of probability would not treat
## each face equally.

## define the sample space (possible outcomes) of a die roll
die_1 <- c(1,2,3,4,5,6)

## define the relative probability of each outcome (here, equally likely) Note
## that these are weights used in the sample() function below. They are not true
## probabilities. You can tell they're not true probabilities because they don't
## sum up to 1.
p_die_1 <- rep(1, 6)

## specify the number of times we'd like to roll the die
n_rolls <- 1

## roll the die
sample(die_1, n_rolls, replace=TRUE, prob=p_die_1)

## We have defined a balanced die (all possible outcomes are equally likely).
## Does that mean that, if we roll the die a bunch of times, we will always get
## equal proportions of each outcome? The answer is definitely no.

## Perform 5 experiments, and for each experiment, roll the die 50 times.
## TODO: take a min here to explain the magical "for" loop
n_exps <- 5
n_rolls <- 50
exp_number <- c()
exp_results <- c()
for(i in 1:n_exps) {
  exp_number <- c(exp_number, rep(i, n_rolls))
  exp_results <- c(exp_results, sample(die_1, n_rolls, replace=TRUE, prob=p_die_1))
}

## convert results from vectors to data.table
d_50 <- data.table('exp_number'=exp_number, 'exp_results'=exp_results)

## plot the results separately for each experiment
ggplot(d_50, aes(x=exp_results)) +
  geom_bar() +
  facet_wrap(~exp_number, ncol=5) +
  theme(aspect.ratio = 1)

## The point is to see that experiment outcomes are variable. Even when we the
## underlying probability of each possible event is equal If you didn't know I
## programmed them this way, and all I gave you was the figure at the end, could
## you guess whether or not it was a balanced die? How about if I told you that
## it was unbalanced, and then offered you a million dollars to guess the
## probability of each outcome? Well, in previous lectures and homework, you
## have seen that the proportion of times an event occurs can be a reasonable
## estimate for its probability. That is, A rational approach is to roll the die
## a bunch of times, and record the proportion of times each face comes up. But
## what about the VARIABILITY, you say? Turns out we can vastly reduce the
## effect of variability by rolling the die LOTS of times.

## First, lets repeat the above experiment, but with many more rolls per experiment
## Perform 5 experiments, and roll the die 10000 times in each.
n_exps <- 5
n_rolls <- 10000
exp_number <- c()
exp_results <- c()
for(i in 1:n_exps) {
  exp_number <- c(exp_number, rep(i, n_rolls))
  exp_results <- c(exp_results, sample(die_1, n_rolls, replace=TRUE, prob=p_die_1))
}

## convert results from vectors to data.table
d_10000 <- data.table('exp_number'=exp_number, 'exp_results'=exp_results)

## plot the results
ggplot(d_10000, aes(x=exp_results)) +
  geom_bar() +
  facet_wrap(~exp_number, ncol=5) +
  theme(aspect.ratio = 1)

## Seems to me like the results given 10000 rolls are much less variable than
## the results given only 50 rolls. Lets compare them side by side.
d_50[, n_rolls := 50]
d_10000[, n_rolls := 10000]
d <- rbind(d_50, d_10000)

## Note the new magic in the facet_wrap() call below.
ggplot(d, aes(x=exp_results)) +
  geom_bar() +
  facet_wrap(~n_rolls*exp_number, ncol=5, scales = 'free') +
  theme(aspect.ratio = 1)

## This is a fine way of looking at things, especially if things stay really
## simple, but we have already learned better methods for quantifying and
## examining variability in experiments... ERROR BARS!
d[, N := .N, .(n_rolls, exp_number, exp_results)]

dd <- d[, .(mean(N), sd(N)/sqrt(.N)), .(exp_results, n_rolls)]
setnames(dd, c('V1','V2'), c('mean','err'))

## In this plot, notice that 10000 rolls leads to more equal outcomes and
## smaller error bars. What does this mean? Would you be ore willing to bet on
## the outome of an experiment that rolled a die 50 times or an experiment that
## rolled a die 10000 times?
ggplot(dd, aes(x=exp_results, y=mean)) +
  geom_bar(stat='identity') +
  geom_errorbar(aes(ymin=mean-err, ymax=mean+err, width=.25)) +
  facet_wrap(~n_rolls, scale='free') +
  theme(aspect.ratio = 1)

## TODO: Take a min to add another n_rolls condition to this mix... pick a value
## for n_rolls that is in between our current values.

## Now, lets try it with a lop sided die
die_2 <- c(1,2,3,4,5,6)

## p1 is the probability of outcomes 1 thru 5
p1 = .75 / 5

## p2 is the probability of outcome 6
p2 = .25

## Make sure that the probabilities of all possible outcomes sums to 1
5*p1 + p2

## All looks good so save p's in a vector
p_die_2 <- c(p1,p1,p1,p1,p1,p2)

n_exps <- 5
n_rolls <- 10000
exp_number <- c()
exp_results <- c()
for(i in 1:n_exps) {
  exp_number <- c(exp_number, rep(i, n_rolls))
  exp_results <- c(exp_results, sample(die_2, n_rolls, replace=TRUE, prob=p_die_2))
}

## TODO: Quick intuition check. How different are p1 and p2? How big an effect
## are you expecting this to have?

d <- data.table('exp_number'=exp_number, 'exp_results'=exp_results)
ggplot(d, aes(x=exp_results)) +
  geom_bar() +
  facet_wrap(~exp_number, ncol=5)

## TODO: Try this experiment out again with different p1 and p2 values and
## compare the results. How different do p1 and p2 need to be before you would
## feel confident accusing somebody of cheating with a lop sided die? What value
## of n_rolls does this hold for? How does your intuition change if n_rolls is
## smaller... say somewhere in the ballpark of what you might actually observe
## in real life?


## Lets dive deeper into the "role" of "n_rolls" --- see what I did there?
## You're welcome.

## An estimate of the probability of each event is the proportion of times that
## event occurs in a sample (result of an experiment)
d[, p_est := .N/n_rolls, .(exp_number, exp_results)]

## We know the 6th side is the heavy side, so lets focus on that for now.
## Recall that for a fair die, P(1)=P(2)=...=P(6) = 1/6 = 0.167
d[exp_results==6, unique(p_est), .(exp_number)]

## Lets investigate more formally how sample size effects our estimate of P(6)
## TODO: Note that, in my opinion, the following bit of coding is fairly clever.
## Go slow and think carefully about it.
n_rolls <- seq(10,500,5)
n_exps <- length(n_rolls)
exp_number <- c()
exp_results <- c()
for(i in 1:n_exps) {
  exp_number <- c(exp_number, rep(i, n_rolls[i]))
  exp_results <- c(exp_results, sample(die_2, n_rolls[i], replace=TRUE, prob=p_die_2))
}

d <- data.table('exp_number'=exp_number, 'exp_results'=exp_results)
d[, n_rolls := .N, .(exp_number)]
d[, p_est := .N/n_rolls, .(exp_number, exp_results)]

## This next figure should illustrate that variability of our p_est decreases as
## n_rolls increases.
ggplot(d[exp_results==6], aes(x=n_rolls, y=p_est)) +
  geom_point() +
  geom_hline(yintercept=p2, colour='red') +
  ylim(0,.5) +
  theme(aspect.ratio = 1)

## Note that the red line is the true value of p2 (we choose this value above).
## Our estimate of p2 is p_est (the relative frequency or proportion of times we
## observed the event of interest). We see here that p_est seems to converge
## towards p2. This is good. It means that using relative frequency as an
## estimate of p2 is sane.

## TODO: Pay attention to the last paragraph.
## (1) There is truth of the world: p2
## (2) There is our estimate of the world: p_est
## Our estimate is based on data, and it is unlikely to be exactly equal to p2
## because the world that we are measuring (rolling die) is a random process.

## NOTE: Important laws of probability for a general sample space
## P(A) = 1 - P(All other possible events)
## P(A and B) = P(A U B) = P(A) + P(B) - P(AB)
