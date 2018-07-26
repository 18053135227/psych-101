## TODO: One important issue with this testing method is that it is highly
## sensitive to non-normal X distributions... allegedly
## TODO: Possible HW to explore varx as x is non-normal
## ## NOTE: If X ~ Uniform(a, b):
## a <- -100
## b <- 100
## n <- c(2,5,10)
## d_rec <- list()
## for(j in n) {
##   for(i in 1:5000) {
##     x <- runif(j, a, b)
##     varx <- var(x)
##     d_rec[[i + j*1000]] <- data.table(exp=i, n=j, varx)
##   }
## }

## d <- rbindlist(d_rec)

## ## Small sample size (small values of n) are greatly skewed (not ~ N)
## ggplot(d, aes(varx)) +
##   geom_histogram(aes(y=..density..), bins=50) +
##   geom_density(colour='red') +
##   facet_wrap(~n) +
##   theme(aspect.ratio = 1)


## ## NOTE: If X ~ Exp(lambda):
## lambda <- 1.5
## n <- c(2,5,10)
## d_rec <- list()
## for(j in n) {
##   for(i in 1:5000) {
##     x <- rexp(j, lambda)
##     varx <- var(x)
##     d_rec[[i + j*1000]] <- data.table(exp=i, n=j, varx)
##   }
## }

## d <- rbindlist(d_rec)

## ## Small sample size (small values of n) are greatly skewed (not ~ N)
## ggplot(d, aes(varx)) +
##   geom_histogram(aes(y=..density..), bins=50) +
##   geom_density(colour='red') +
##   facet_wrap(~n) +
##   theme(aspect.ratio = 1)
