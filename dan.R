## ----setup, echo = FALSE-------------------------------------------------
knitr::opts_chunk$set(
  warnings = FALSE,
  fig.width = 12, 
  fig.height = 4, 
  fig.path = 'img/')

## ----visible-setup, size = "small"---------------------------------------
set.seed(54321) ## because reproducibility matters...
suppressMessages(library(R.utils)) ## make sure it is installed
expit <- plogis
logit <- qlogis

## ----simulation, size = "small"------------------------------------------
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
  ## ## actual rewards
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

## ----draw-five-obs, size = "small"---------------------------------------
five.obs <- drawFromExperiment(5) 
five.obs

## ----DAG-----------------------------------------------------------------
## plot the causal diagram

## ----approx-psi-one------------------------------------------------------
B <- 1e6
full.obs <- drawFromExperiment(B, full = TRUE)
psi.hat <- mean(full.obs[, "Yone"] - full.obs[, "Yzero"])
psi.hat

## ----approx-psi-two------------------------------------------------------
sd.hat <- sd(full.obs[, "Yone"] - full.obs[, "Yzero"])
alpha <- 0.05
psi.CI <- psi.hat + c(-1, 1) * qnorm(1 - alpha / 2) * sd.hat / sqrt(B)
psi.CI

