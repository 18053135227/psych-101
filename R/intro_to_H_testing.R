library('data.table')
library('ggplot2')

generate_data_normal <- function(mu, sig, n) {
  ## TODO: This raises an interesting point... different families are defined by
  ## different parameters...

  ## TODO: define possible families:
  ## Normal
  ## Binomial
  ## Geometric
  ## Hypergeometric
  ## Poisson distribution

  ## if(missing(family)) {}
  if(missing(mu)) {
    mu <- runif(1, 0, 100)
  }

  if(missing(sig)) {
    sig <- runif(1, 0, 100)
  }

  data <- rnorm(n, mean=mu, sd=sig)

  return(data)
}
