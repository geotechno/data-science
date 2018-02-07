## Required packages.
library("R2BayesX")
library("maps")

## Read the data.
d <- read.csv("earthquakes.csv")

## Create a map of observed earth quakes.
rlon <- range(d$longitude)
rlat <- range(d$latitude)

map("world", xlim = rlon, ylim = rlat)
points(d$longitude, d$latitude, pch = 4, col = "red")
map("world", add = TRUE)

## Create additional time variables from dates.
dtime <- sapply(strsplit(as.character(d$time), "T"),
  function(x) { x[1] })
dtime <- as.Date(dtime, format = "%Y-%m-%d")
d$yday <- as.POSIXlt(dtime)$yday
d$year <- (as.POSIXlt(dtime)$year + 1900)
d$trend <- d$year + d$yday / 366

## Estimate model.
b <- bam(mag ~ s(trend) + s(depth) + s(longitude, latitude, k = 200), data = d)

## Plot smooth functions.
plot(b)

## Predict spatial effect
## to create a "risk" map.
n <- 200
nd <- expand.grid(
  "longitude" = seq(rlon[1], rlon[2], length = n),
  "latitude" = seq(rlat[1], rlat[2], length = n)
)
nd$trend <- max(d$trend)
nd$depth <- mean(d$depth, na.rm = TRUE)

nd$p <- predict(b, newdata = nd, type = "terms")[, "s(longitude,latitude)"]

## Plot the map.
riskmap <- function(lon, lat, x, color = diverge_hcl, ncol = 99,
  map = TRUE, ...)
{
  require("R2BayesX")

  op <- par(no.readonly = TRUE)
  on.exit(par(op))

  dx <- diff(lon)
  dy <- diff(lat)
  dx <- min(dx[dx != 0])
  dy <- min(dy[dy != 0])

  pal <- colorlegend(color = color, x = x, ncol = ncol, plot = FALSE, ...)

  mar <- par()$mar
  par(mar = mar, xaxs = "i")
  w <- (4.5 + mar[4L]) * par("csi") * 1.7

  layout(matrix(c(1, 2), nrow = 1), widths = c(1, lcm(w)))
  par(mar = c(4.1, 4.1, mar[3], 0.1))
  plot(range(lon) + c(-dx/2, dx/2), range(lat) + c(-dy/2, dy/2), type = "n",
    xlab = "Longitude [deg]", ylab = "Latitude [deg]")

  rect(lon - dx/2, lat - dy/2, lon + dx/2, lat + dy/2, col = pal$map(x), border = NA)
  box()
  if(map)
    map("world", add = TRUE)
  par(mar = c(4.1, 0.5, mar[3], mar[4]))
  colorlegend(color = color, x = x, ncol = ncol, plot = TRUE, full = TRUE,
    side.legend = 2, side.ticks = 2, ...)
}

with(nd, riskmap(longitude, latitude, p, range = c(-0.5, 0.5)))
with(nd, riskmap(longitude, latitude, p + coef(b)[1], color = heat_hcl,
  symmetric = FALSE, swap = TRUE,
  range = quantile(p + coef(b)[1], probs = c(0.1, 0.9))))

