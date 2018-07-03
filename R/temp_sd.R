library(data.table)
library(ggplot2)

rm(list=ls())

d <- as.data.table(iris)

## dostuff takes a data.table of type iris
dostuff <- function(z) {
  print(z[, Species])
  print(z)
}

d[, dostuff(.SD), .(Species), .SDcols=c('Species', 'Sepal.Length')]

d[, .SD]
d[, print(.SD)]
