library('data.table')
library('ggplot2')


## NOTE: Analyze rbbsdelay.csv
rm(list = ls())

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
options(contrasts = c("contr.sum","contr.poly"))


## NOTE: Is there a difference between any conditions in the mean accuracy
## achieved during the first 16 blocks?
ddd <- d[block <= 16, mean(acc_mean), .(condition, subject)]
setnames(ddd, 'V1', 'acc')

dddd <- ddd[, .(mean(acc), sd(acc)/sqrt(.N)), .(condition)]
setnames(dddd, c('V1','V2'), c('acc','err'))
ggplot(dddd, aes(x=condition, y=acc)) +
  geom_bar(stat='identity') +
  geom_errorbar(aes(ymin=acc-err, ymax=acc+err), width=.25) +
  theme(aspect.ratio = 1)
## TODO: introduce ggsave()
ggsave('../figures/test_fig.pdf', width=5, height=5)

## One-way ANOVA
fm <- lm(acc ~ condition, data=ddd)
anova(fm)

x <- ddd[condition=='delay', acc]
y <- ddd[condition=='immed', acc]
z <- ddd[condition=='short', acc]

t.test(x, y)
t.test(x, z)
t.test(y, z)

## TODO:
## Q: Why use ANOVA to assess differences between multiple treatments /
## conditions / whatever when you could just use multiple t-tests?


## NOTE: Are there any differences between any of the conditions across any
## blocks that are less than or equal to 16?
## Two-way ANOVA

## plot main effect of condition
dd <- d[block<=16, .(mean(acc_mean), sd(acc_mean)/sqrt(.N)), .(condition)]
setnames(dd, c('V1','V2'), c('acc','err'))
ggplot(dd, aes(x=condition, y=acc)) +
  geom_bar(stat='identity') +
  geom_errorbar(aes(ymin=acc-err, ymax=acc+err), width=.25) +
  theme(aspect.ratio = 1)

## plot main effect of block
dd <- d[block<=16, .(mean(acc_mean), sd(acc_mean)/sqrt(.N)), .(block)]
setnames(dd, c('V1','V2'), c('acc','err'))
ggplot(dd, aes(x=block, y=acc)) +
  geom_bar(stat='identity') +
  geom_errorbar(aes(ymin=acc-err, ymax=acc+err), width=.25) +
  theme(aspect.ratio = 1)

## plot condition*block interaction
## This not easy to interpret in for this example, and would not typically be
## used for much insight here.
dd <- d[block<=16, .(mean(acc_mean), sd(acc_mean)/sqrt(.N)), .(condition, block)]
setnames(dd, c('V1','V2'), c('acc','err'))
ggplot(dd, aes(x=block, y=acc)) +
  geom_bar(stat='identity') +
  geom_errorbar(aes(ymin=acc-err, ymax=acc+err), width=.25) +
  theme(aspect.ratio = 1) +
  facet_wrap(~condition)

fm <- lm(acc_mean ~ condition*block, data=d[block <= 16])
anova(fm)

## TODO: introduce aov
fm <- aov(acc_mean ~ condition*block, data=d[block <= 16])
summary(fm)


## NOTE: Are there any differences between any of the conditions across any
## blocks that are greater than 16?
## Two-way ANOVA
fm <- lm(acc_mean ~ condition*block, data=d[block > 16])
anova(fm)


## NOTE:
## Two-way ANOVA
fm <- lm(acc_mean ~ condition*block, data=d[block %in% c(16,17)])
anova(fm)

dd <- d[block %in% c(16,17),
        .(mean(acc_mean), sd(acc_mean)/sqrt(.N)),
        .(condition, block)]
setnames(dd, c('V1','V2'), c('acc','err'))

## The interaction plot will make more sense here
ggplot(dd, aes(x=block, y=acc, colour=condition)) +
  geom_point() +
  geom_line() +
  geom_errorbar(aes(ymin=acc-err, ymax=acc+err), width=.05) +
  theme(aspect.ratio = 1)


###########################################################################
###########################################################################
###########################################################################

## TODO: CODE THIS LIVE

## NOTE: Analyze switch_data.csv
rm(list = ls())
f <- '../data/switch_cat_learn/switch_data.csv'
d <- fread(f)

## how many subjects per condition here?
d[, length(unique(subject)), .(condition)]

## We do not have a balanced design, so be sure to set sum of squares type to
## type III --- the standard used in psychology
options(contrasts = c("contr.sum","contr.poly"))

## NOTE: Plot it
dd <- d[, .(mean(acc), sd(acc)/sqrt(.N)), .(condition, phase, block, cue)]
setnames(dd, c('V1','V2'), c('acc_mean', 'acc_err'))
ggplot() +
  geom_line(data=dd[phase==1], aes(x=block, y=acc_mean, colour=factor(cue))) +
  geom_errorbar(data=dd[phase==1],
                aes(x=block,
                    ymin=acc_mean-acc_err,
                    ymax=acc_mean+acc_err,
                    colour=factor(cue))) +
  geom_line(data=dd[phase==2], aes(x=block, y=acc_mean, colour=factor(cue))) +
  geom_errorbar(data=dd[phase==2],
                aes(x=block,
                    ymin=acc_mean-acc_err,
                    ymax=acc_mean+acc_err,
                    colour=factor(cue))) +
  geom_line(data=dd[phase==3], aes(x=block, y=acc_mean, colour=factor(cue))) +
  geom_errorbar(data=dd[phase==3],
                aes(x=block,
                    ymin=acc_mean-acc_err,
                    ymax=acc_mean+acc_err,
                    colour=factor(cue))) +
  geom_line(data=dd[phase==4], aes(x=block, y=acc_mean, colour=factor(cue))) +
  geom_errorbar(data=dd[phase==4],
                aes(x=block,
                    ymin=acc_mean-acc_err,
                    ymax=acc_mean+acc_err,
                    colour=factor(cue))) +
  facet_wrap(~condition)


## NOTE: do some stats
dd <- d[, .(mean(acc)), .(condition, phase, block, subject, cue)]
setnames(dd, c('V1'), c('acc_mean'))
fm <- lm(acc_mean ~ condition*block*cue, data=dd)
anova(fm)

## significant effect of condition
ddd <- dd[, .(mean(acc_mean), sd(acc_mean)/sqrt(.N)), .(condition)]
setnames(ddd, c('V1','V2'), c('acc_mean','acc_err'))

## significant effect of cue
ddd <- dd[, .(mean(acc_mean), sd(acc_mean)/sqrt(.N)), .(cue)]
setnames(ddd, c('V1','V2'), c('acc_mean','acc_err'))

## significant condition:block interaction
ddd <- dd[, .(mean(acc_mean), sd(acc_mean)/sqrt(.N)), .(condition, block)]
setnames(ddd, c('V1','V2'), c('acc_mean','acc_err'))
