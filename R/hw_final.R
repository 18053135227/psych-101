library('data.table')
library('ggplot2')

## NOTE 1: prep the data

## (a) load rbbsdelay.csv into a data.table

## (b) plot mean accuracy over all subjects grouped by condition on the y-axis
## against block on the x-axis. Use different colors for different conditions.
## Also, add a dashed vertical line between blocks 16 and 17.


## NOTE 2: Use NHST to answer the following questions. Unless otherwise specified,
## answer each question seperately for each condition. You can use one-liner
## test functions to do these if you like.

## (1) Is the mean accuracy on block 1 greater than chance?

## (2) Is the mean accuracy on block 17 worse than it was on block 16?

## (3) Is the mean accuracy over the first 16 blocks lower in the "short"
##     condition than is was in the "delay" condition?

## (4) Is the variance in mean accuracy during block 16 greater than 0.01?

## (5) Is the variance in mean accuracy during block 16 greater than the variance
##     during block 17?

## (6) Is there a significant difference in mean accuracy between any of the
##     three conditions during the final 8 blocks?

## (7) Is the mean reaction time on block 1 greater than chance?

## (8) Is the mean reaction time on block 17 worse than it was on block 16?

## (9) Is the mean reaction time over the first 16 blocks lower in the "short"
##     condition than is was in the "delay" condition?

## (10) Is the variance in mean reaction time during block 16 greater than 0.01?

## (11) Is the variance in mean reaction time during block 16 greater than the variance
##      during block 17?

## (12) Is there a significant difference in mean reaction time between any of the
##      three conditions during the final 8 blocks?


## NOTE 3: Why is there a big drop in accuracy between block 16 and 17?


## NOTE 4: Does accuracy on block 16 predict accuracy on block 17?
