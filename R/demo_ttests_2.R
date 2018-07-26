library('data.table')
library('ggplot2')

rm(list = ls())
f <- '../data/switch_cat_learn/switch_data.csv'
d <- fread(f)

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


## NOTE: Classifiy the following factors as within-subject or as
## between-subjects factors

## condition
## measurements from different levels of condition come from different subjects,
## so condition is a between-subjects factor.

## phase
## measurements from different levels of phase (if all other factors are held
## constant) come from the same subjects, so phase is a within-subjects factor.

## block
## block is within-subjects. Same logic as phase.

## trial
## trial is within-subjects. Same logic as phase.


## NOTE: Independent Samples
## Is there a difference in accuracy between condition 1 and condition 2 during
## phase 1?

## X = RV that generates data for condition 1
## Y = RV that generates data for condition 2

## X and Y are independent

## H0: muX - muY = 0
## H1: muX - muY != 0
mu_z_bar_null <- 0

## Step 2:
alpha  <- 0.05

## Step 3:
## Get observations from X and Y
x <- d[condition==1 & phase==1, mean(acc), .(phase, subject)][, V1]
y <- d[condition==2 & phase==1, mean(acc), .(phase, subject)][, V1]
nx <- length(x)
ny <- length(y)

## Get observations from X_bar, Y_bar, and Z_bar = X_bar - Y_bar
x_bar_obs <- mean(x)
y_bar_obs <- mean(y)
z_bar_obs <- x_bar_obs - y_bar_obs

## Estimate the variance and standard deviation of the Z_bar distribution
var_z_bar_obs <- var(x)/nx + var(y)/ny
sigma_z_bar_obs <- sqrt(var_z_bar_obs)

## Step 4:
df = ((sd(x)^2)/nx + (sd(y)^2)/ny)^2 /
    ((1/(nx-1))*((sd(x)^2)/nx)^2 + (1/(ny-1))*((sd(y)^2)/ny)^2)

t_obs <- (z_bar_obs - mu_z_bar_null) / sigma_z_bar_obs

## TODO: Draw on the board to illustrate what's going on with the p-value
p_obs_lower <- pt(-abs(t_obs), df, lower.tail = TRUE)
p_obs_upper <- pt(abs(t_obs), df, lower.tail = FALSE)
t_crit_lower <- qt(alpha/2, df, lower.tail = TRUE)
t_crit_upper <- qt(alpha/2, df, lower.tail = FALSE)

## Step 5:
if(p_obs_less < alpha/2 | p_obs_greater < alpha/2) {
    print('Reject H0')
} else {
    print('Fail to reject H0')
}

if(t_obs > t_crit_upper| t_obs < t_crit_lower) {
    print('Reject H0')
} else {
    print('Fail to reject H0')
}

## Do it all via built-in t.test() function
t.test(x,
       y,
       alternative='two.sided',
       mu=mu_Z_bar_null,
       paired=FALSE,
       var.equal=FALSE,
       conf.level=.95)


## NOTE: show interaction with t.test object
tresult <- t.test(x,
                  y,
                  alternative='two.sided',
                  mu=mu_Z_bar_null,
                  paired=FALSE,
                  var.equal=FALSE,
                  conf.level=.95)

class(tresult)
str(tresult)
tresult['statistic']
tresult$statistic
tresult['p.value']
tresult$p.value



## NOTE: Paired samples
## Is there a difference between the accuracy during block 2 and block 3 of
## condition 1.

## X = RV that generates data for block 2
## Y = RV that generates data for block 3

## X and Y are not independent (because block is a within-subjects factor)

## Step 1:
## muD = 0
## muD != 0
muD_null <- 0

## Step 2:
alpha <- 0.05

## Step 3:
## get raw observations from X and Y
x_obs <- d[condition==1 & block==2, mean(acc), .(subject)][, V1]
y_obs <- d[condition==1 & block==3, mean(acc), .(subject)][, V1]

## get raw observations from D
d_obs <- x_obs - y_obs

## compute our test statistic (i.e., estimator for the popoulation mean of D)
d_bar_obs <- mean(d_obs)

## estimate the standard deviation of our sampling distribution (D_bar)
n <- length(d_obs)
d_bar_sigma_obs <- sd(d_obs) / sqrt(n)

## Compute t_obs
t_obs <- (d_bar_obs - muD_null) / d_bar_sigma_obs

## Step 4:
## Compute p-values and critical values
df <- n-1
p_obs_lower <- pt(-abs(t_obs), df, lower.tail=TRUE)
p_obs_upper <- pt(abs(t_obs), df, lower.tail=FALSE)
t_crit_lower <- qt(alpha/2, df, lower.tail=TRUE)
t_crit_upper <- qt(alpha/2, df, lower.tail=FALSE)

## Step 5:
if(p_obs_lower < alpha/2 | p_obs_upper < alpha/2) {
  print('Reject H0')
} else {
  print('Fail to reject H0')
}

if(t_obs > t_crit_upper| t_obs < t_crit_lower) {
  print('Reject H0')
} else {
  print('Fail to reject H0')
}

## Doing this via built-in t.test() function
## Option 1
t.test(x=x_obs,
       y=y_obs,
       alternative='two.sided',
       mu=muD_null,
       paired=TRUE,
       var.equal=FALSE,
       conf.level=1-alpha)

## Doing this via built-in t.test() function
## Option 1
t.test(x=d_obs,
       y=NULL,
       alternative='two.sided',
       mu=muD_null,
       paired=FALSE,
       var.equal=FALSE,
       conf.level=1-alpha)


## NOTE: A few examples interacting with the final project data
rm(list = ls())

dir_data_delay = '../final_project/data/delay/'
files_delay <- list.files(dir_data_delay)

d_rec <- list()
for(i in 1:length(files_delay)) {
  f_name <- files_delay[i]
  f <- paste(dir_data_delay, f_name, sep='')
  d <- fread(f)
  d[, fname := f_name]
  d[, condition := 'delay']
  d_rec[[i]] <- d
}

d <- rbindlist(d_rec)

col_names <- c(
  't',
  't_prob',
  'bnd',
  'cat',
  'x',
  'y',
  'rsp',
  'rt'
)

setnames(d, 1:8, col_names)

d[, acc := cat == rsp]


## NOTE: Did they guess during the first 20 trials? --- Binomial approach
## 1.
## H0: p = .5
## H1: p > .5
p_H0 <- .5

## 2.
alpha <- .05

## 3.
## X ~ Bernoulli(p)
## X_obs ~ binom(n, p)
x <- d[, acc[1:20], .(fname)][, V1]
n <- length(x)
x_obs <- sum(x, na.rm=TRUE) ## TODO: address the NAs

## 4. P(X > x_obs)
p_obs <- pbinom(x_obs-1, n, p_H0, lower.tail=FALSE)
x_crit <- qbinom(alpha, n, p_H0, lower.tail=FALSE)

## 5.
p_obs < alpha
x_obs > x_crit

## using binom.test
binom.test(x_obs, n, p_H0, alternative='greater')


## NOTE: Did they guess during the first 20 trials? --- Normal approach
## 1.
## H0: mu = .5
## H1: mu > .5
muH0 <- .5

## 2.
alpha <- .05

## 3.
## X ~ N(muX, sigmaX)
## X_obs ~ N(muX, sigmaX/sqrt(n))

## Estimate the population parameter mu
x <- d[, mean(acc[1:20]), .(fname)][, V1]
## x <- x[!is.na(x)] ## NOTE: an alternative to na.rm = TRUE
x_obs <- mean(x, na.rm=TRUE)
n <- length(x)
df <- n - 1

## 4. P(t > t_obs)
t_obs  <- (x_obs - muH0) / (sd(x, na.rm=TRUE)/sqrt(n))
p_obs <- pt(t_obs, df, lower.tail=FALSE)
t_crit <- qt(alpha, df, lower.tail=FALSE)

## 5.
p_obs < alpha
t_obs > t_crit

## using t.test
t.test(x, mu=0.5, alternative='greater', na.rm=TRUE)


## NOTE: Is the mean number of problems solved (NPS) less than 8?
## TODO: Unpack the next line to understand what it's doing
nps <- d[, sum(t_prob == 1)-1, .(fname)]

## 1.
## H0: mu = 8
## H1: mu < 8
mu_x_H0 <- 8

## 2.
alpha <- 0.05

## 3.
## X ~ N(muX, sigmaX)
## X_obs ~ N(muX, sigmaX/sqrt(n))
x <- nps[, V1]
x_obs <- mean(x)

n <- length(x)
sig_x_obs <- sd(x) / sqrt(n)
t_obs <- (x_obs - mu_x_H0) / sig_x_obs

## 4.
df <- n-1
p_H0 <- pt(t_obs, df, lower.tail=TRUE)
t_crit <- qt(alpha, df, lower.tail=TRUE)

## 5.
p_obs < alpha
t_obs < t_crit

## use t.test
t.test(x, mu=mu_x_H0, alternative='less')


## NOTE: Is the mean trials to criterion (t2c) > 60?
## TODO: Unpack the next line to understand what it's doing
t2c <- d[, mean(diff(which(t_prob == 1))), .(fname)]

## inspect suspicious NaNs
t2c[fname=='delay_fb_sub217.txt']
d[fname=='delay_fb_sub217.txt'] ## NOTE: never solved a single problem

## For now, simply remove them from consideraation
t2c <- t2c[is.finite(V1)]

## 1.
## H0: mu = 60
## H1: mu > 60
mu_x_H0 <- 60

## 2.
alpha <- 0.05

## 3.
## X ~ N(muX, sigmaX)
## X_obs ~ N(muX, sigmaX/sqrt(n))
x <- t2c[, V1]
x_obs <- mean(x)

n <- length(x)
sig_x_obs <- sd(x) / sqrt(n)
t_obs <- (x_obs - mu_x_H0) / sig_x_obs

## 4.
df <- n-1
p_obs <- pt(t_obs, df, lower.tail=FALSE)
t_crit <- qt(alpha, df, lower.tail=FALSE)

## 5.
p_obs < alpha
t_obs > t_crit

## use t.test
t.test(x, mu=mu_x_H0, alternative='greater')
