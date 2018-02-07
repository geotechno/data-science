################
## Exercise 1 ##
################
## Read initial data set.
d <- read.table("Messina.txt")

## Add identifier variable id.
d2 <- cbind(d, "id" = rep(1:2, nrow(d)))

## Extract time and magnitude with subset().
time <- subset(d2, id == 1)
magnitude <- subset(d2, id == 2)

## Create nice data.frame
Messina <- data.frame("time" = time$V1, "magnitude" = magnitude$V1)

## Sort the data by time.
Messina <- Messina[order(Messina$time), ]

## Compute the interarrival times.
Messina$itimes <- c(NA, diff(Messina$time))

## Create nice function that does the plotting.
m.plot <- function(data, ...)
{
  ## Plot the histogram of interarrival times.
  hist(data$itimes, freq = FALSE, ...)

  ## Add kernel density estimate to histogram plot.
  lines(density(na.omit(data$itimes), from = 0.1))

  ## Add analytical exponential density function.
  x <- seq(min(data$itimes, na.rm = TRUE), max(data$itimes, na.rm = TRUE), length = 100)
  lines(dexp(x, rate = 1 / mean(data$itime, na.rm = TRUE)) ~ x,
    col = rgb(0.8, 0, 0), lwd = 3)
}

## Plot 2 in 1 plots.
## Save plot as .png.
save <- FALSE
if(save)
  png("Messina.png", units = "in", width = 10, height = 4, res = 120)

par(mfrow = c(1, 2))

## Plot.
m.plot(Messina,
  xlab = "Interarrival times", main = "Full data",
  ylim = c(0, 0.45))

## Plot for magnitude > 5.
m.plot(subset(Messina, magnitude > 5),
  xlab = "Interarrival times", main = "Magnitude > 5",
  ylim = c(0, 0.45))

if(save)
  dev.off()


################
## Exercise 2 ##
################
## Get the data, rain is metric/ration scale, has logical zero and
## taking differences makes sence.
rain <- c(2, 9, 18, 2, 23, 42, 11, 13, 40, 12, 50, 12, 0, 0, 0, 0, 3, 3, 40, 48)

## Histogram with pre-specified breaks.
hist(rain, breaks = seq(0, 50, by = 10), right = FALSE)

## Plot of empirical cumulative distribution function.
ecdf_rain <- ecdf(rain)
plot(ecdf_rain)

## The nice thing is that ecdf() returns a function,
## we can directly compute the desired percentage rate.
ecdf_rain(35)
1 - ecdf_rain(35)

## Computing measures of location and dispersion.
median(rain)
quantile(rain)

## quantile() with type = 2 corresponds to the
## quantiles from the slides.
quantile(rain, type = 2)
sd(rain)
mean(rain)
range(rain)

## A simple boxplot.
boxplot(rain)

## A 95% confidence interval for the mean,
## needs the 1- alpha/2 quantile of the
## standard normal distribution.
ci <- round(mean(rain) + c(-1, 1) * qnorm(1 - 0.05/2) * sd(rain) / sqrt(length(rain)), 2)
ci

## Create a character string of ci.
paste("[", ci[1], ", ", ci[2], "]", sep = "")

