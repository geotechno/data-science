## Example: maximum-likelihood estimation
##          for mean and standard deviation
##          using optim().

## Set up the log-likelihood function to be
## used for optimization. Argument theta
## is a vector of length 2, first element
## is the mean, second the standard deviation.
logLik <- function(theta) {
  ## Ensure that standard deviation
  ## is always positive with exp().
  ll <- sum(dnorm(x, mean = theta[1], sd = exp(theta[2]), log = TRUE))

  ## Need to multiply by -1 since optim()
  ## does minimization!
  return(-1 * ll)
}

## Generate some data for testing,
## true mean = 3, and standard
## deviation = 2.
n <- 200
x <- rnorm(n, mean = 3, sd = 2)

## Set starting values and obtain
## maximum-likelihood estimates with optim().
start <- c(0, 0)
opt <- optim(start, fn = logLik, method = "BFGS")

## Plot histogram of the data and compare
## with normal density using estimated parameters.
hist(x, freq = FALSE)
i <- order(x)
lines(dnorm(x[i], mean = opt$par[1], sd = exp(opt$par[2])) ~ x[i],
  col = "green", lwd = 2)
lines(dnorm(x[i], mean = 3, sd = 2) ~ x[i],
  col = "red", lwd = 2)
abline(v = opt$par[1], lty = 2, col = "green", lwd = 2)
abline(v = 3, lty = 2, col = "red", lwd = 2)
legend("topleft", c("fitted normal density", "true density"), lwd = 2,
  col = c("green", "red"), box.col = NA)

