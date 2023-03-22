library(zoo)
I <- function(x) {
  y <- rep(0, length(x))
  for (i in 1:length(x)) {
    if (x[i] == 1) (y[i] <- 1)
  }
  y
}
eta <- seq(0.00001, 0.99991, 0.0001)
L1 <- L2 <- rep(length(eta))
for (k in 1:length(eta)) {
  theta <- eta[k]
  x <- 1:1000
  L1[k] <- sum((I(x) - theta)^2 * theta * (1 - theta)^(x - 1)) / (theta * (1 - theta))
  L2[k] <- sum(((1 / x) - theta)^2 * theta * (1 - theta)^(x - 1)) / (theta * (1 - theta))
}
id <- order(eta)
AUC1 <- sum(diff(eta[id]) * rollmean(L1[id], 2))
AUC2 <- sum(diff(eta[id]) * rollmean(L2[id], 2))
AUC1
AUC2
