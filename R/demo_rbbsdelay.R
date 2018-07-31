library('data.table')
library('ggplot2')

## load the data
d <- fread('../data/rbbsdelay/rbbsdelay.csv')

## quickly plot the data
switch_block <- 16.5
dd <- d[, .(mean(acc_mean), sd(acc_mean)/sqrt(.N)), .(condition, block)]
setnames(dd, c('V1','V2'), c('acc_mean','acc_err'))
ggplot(dd, aes(x=block, y=acc_mean, colour=condition)) +
  geom_line() +
  geom_errorbar(aes(ymin=acc_mean-acc_err, ymax=acc_mean+acc_err)) +
  geom_vline(xintercept = switch_block, linetype=2)

## how many subjects per condition here?
d[, length(unique(subject)), .(condition)]

## We do not have a balanced design, so be sure to set sum of squares type to
## type III --- the standard used in psychology
options(contrasts = c(“contr.sum”,”contr.poly”))

## Is there a difference between any conditions in the mean accuracy achieved
## during the first 16 blocks?
ddd <- d[block <= 16, mean(acc_mean), .(condition, subject)]
setnames(ddd, 'V1', 'acc')

fm <- lm(acc ~ condition, data=ddd)
anova(fm)
