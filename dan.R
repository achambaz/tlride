## ----setup, echo = FALSE-------------------------------------------------
knitr::opts_chunk$set(
  size = "tiny",
  warnings = FALSE,
  fig.width = 12, 
  fig.height = 4, 
  fig.path = 'img/')

## ----visible-setup-------------------------------------------------------
set.seed(54321) ## because reproducibility matters...
suppressMessages(library(R.utils)) ## make sure it is installed
suppressMessages(library(ggplot2)) ## make sure it is installed
expit <- plogis
logit <- qlogis

## ----simulation----------------------------------------------------------
drawFromExperiment <- function(n, full = FALSE) {
  ## preliminary
  n <- Arguments$getInteger(n, c(1, Inf))
  full <- Arguments$getLogical(full)
  ## ## 'gbar' and 'Qbar' factors
  gbar <- function(W) {
    expit(-0.3 + 2 * W - 1.5 * W^2)
  }
  Qbar <- function(AW) {
    A <- AW[, 1]
	W <- AW[, 2]
    A * cos(2 * pi * W) + (1 - A) * sin(2 * pi * W^2)
  }
  ## sampling
  ## ## context
  W <- runif(n)
  ## ## counterfactual rewards
  zeroW <- cbind(A = 0, W)
  oneW <- cbind(A = 1, W)
  Yzero <- rnorm(n, mean = Qbar(zeroW), sd = 1)
  Yone <- rnorm(n, mean = Qbar(oneW), sd = 1)
  ## ## action undertaken
  A <- rbinom(n, size = 1, prob = gbar(W))
  ## ## actual reward
  Y <- A * Yone + (1 - A) * Yzero
  ## ## observation
  if (full) {
    obs <- cbind(W = W, Yzero = Yzero, Yone = Yone, A = A, Y = Y)
  } else {
    obs <- cbind(W = W, A = A, Y = Y)
  }
  attr(obs, "gbar") <- gbar
  attr(obs, "Qbar") <- Qbar
  attr(obs, "QW") <- dunif
  ##
  return(obs)
}

## ----draw-five-obs-------------------------------------------------------
five.obs <- drawFromExperiment(5) 
five.obs

## ----DAG-----------------------------------------------------------------
## plot the causal diagram

## ----approx-psi-zero-a---------------------------------------------------
B <- 1e6
full.obs <- drawFromExperiment(B, full = TRUE)
psi.hat <- mean(full.obs[, "Yone"] - full.obs[, "Yzero"])
psi.hat

## ----approx-psi-zero-b---------------------------------------------------
sd.hat <- sd(full.obs[, "Yone"] - full.obs[, "Yzero"])
alpha <- 0.05
psi.CI <- psi.hat + c(-1, 1) * qnorm(1 - alpha / 2) * sd.hat / sqrt(B)
psi.CI

## ----another_simulation--------------------------------------------------
drawFromAnotherExperiment <- function(n, h = 0) {
  ## preliminary
  n <- Arguments$getInteger(n, c(1, Inf))
  h <- Arguments$getNumeric(h)
  ## ## 'gbar' and 'Qbar' factors
  gbar <- function(W) {
    sin((1 + W) * pi / 6)
  }
  Qbar <- function(AW, hh = h) {
    A <- AW[, 1]
    W <- AW[, 2]
    expit( logit( A *  W + (1 - A) * W^2 ) +
           hh * 10 * sqrt(W) * A )
  }
  ## sampling
  ## ## context
  W <- rbeta(n, shape1 = 2, shape2 = 2)
  ## ## action undertaken
  A <- rbinom(n, size = 1, prob = gbar(W))
  ## ## reward
  QAW <- Qbar(cbind(A, W))
  Y <- rbeta(n, shape1 = 1, shape2 = (1 - QAW) / QAW)
  ## ## observation
  obs <- cbind(W = W, A = A, Y = Y)
  attr(obs, "gbar") <- gbar
  attr(obs, "Qbar") <- Qbar
  attr(obs, "QW") <- dunif
  ##
  return(obs)
}

## ----approx-psi-one------------------------------------------------------
obs.from.another.experiment <- drawFromAnotherExperiment(1)
integrand <- function(w) {
  Qbar <- attr(obs.from.another.experiment, "Qbar")
  QW <- attr(obs.from.another.experiment, "QW")
  ( Qbar(cbind(1, w)) - Qbar(cbind(0, w)) ) * QW(w)
}
psi.Pi.zero <- integrate(integrand, lower = 0, upper = 1)$val
psi.Pi.zero

## ----psi-approx-psi-one--------------------------------------------------
approx <- seq(-1, 1, length.out = 1e2)
psi.Pi.h <- sapply(approx, function(t) {
  obs.from.another.experiment <- drawFromAnotherExperiment(1, h = t)
  integrand <- function(w) {
    Qbar <- attr(obs.from.another.experiment, "Qbar")
    QW <- attr(obs.from.another.experiment, "QW")
    ( Qbar(cbind(1, w)) - Qbar(cbind(0, w)) ) * QW(w)
  }
  integrate(integrand, lower = 0, upper = 1)$val  
})
slope <- (psi.Pi.h - psi.Pi.zero) / approx
slope <- slope[min(which(approx > 0))]
ggplot() +
  geom_point(data = data.frame(x = approx,
                               y = psi.Pi.h),
             aes(x, y), color = "firebrick") +
  labs(x = "h", y = expression(Psi(Pi[h]))) +
  geom_segment(data = data.frame(x = c(-1, 1),
                                 y = psi.Pi.zero + slope * c(-1, 1)),
               aes(x = x[1], y = y[1], xend = x[2], yend = y[2]),
               color = "blue",
               arrow = arrow(length = unit(0.03, "npc"))) +
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = psi.Pi.zero)

