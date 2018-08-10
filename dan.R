## ----setup, echo = FALSE-------------------------------------------------
knitr::opts_chunk$set(
  fig.height = 4, 
  fig.path = 'img/',
  fig.width = 12,
  message = FALSE,
  size = "tiny",
  warning = FALSE,
  warnings = FALSE
)

## ----visible-setup-------------------------------------------------------
set.seed(54321) ## because reproducibility matters...
suppressMessages(library(R.utils)) ## make sure it is installed
suppressMessages(library(tidyverse)) ## make sure it is installed
suppressMessages(library(ggplot2)) ## make sure it is installed
expit <- plogis
logit <- qlogis

## ----simulation----------------------------------------------------------
draw_from_experiment <- function(n, full = FALSE) {
  ## preliminary
  n <- Arguments$getInteger(n, c(1, Inf))
  full <- Arguments$getLogical(full)
  ## ## 'Gbar' and 'Qbar' factors
  Gbar <- function(W) {
    expit(-0.2 + 3 * sqrt(W) - 1.5 * W)
  }
  Qbar <- function(AW) {
    A <- AW[, 1]
    W <- AW[, 2]
    A * cos((1 + W) * pi / 5) + (1 - A) * sin((1 + W^2) * pi / 4)
  }
  ## sampling
  ## ## context
  W <- runif(n)
  ## ## counterfactual rewards
  zeroW <- cbind(A = 0, W)
  oneW <- cbind(A = 1, W)
  Qbar.zeroW <- Qbar(zeroW)
  Qbar.oneW <- Qbar(oneW)
  Yzero <- rbeta(n, shape1 = 1, shape2 = (1 - Qbar.zeroW) / Qbar.zeroW)
  Yone <- rbeta(n, shape1 = 1, shape2 = (1 - Qbar.oneW) / Qbar.oneW)
  ## ## action undertaken
  A <- rbinom(n, size = 1, prob = Gbar(W))
  ## ## actual reward
  Y <- A * Yone + (1 - A) * Yzero
  ## ## observation
  if (full) {
    obs <- cbind(W = W, Yzero = Yzero, Yone = Yone, A = A, Y = Y)
  } else {
    obs <- cbind(W = W, A = A, Y = Y)
  }
  attr(obs, "Gbar") <- Gbar
  attr(obs, "Qbar") <- Qbar
  attr(obs, "QW") <- dunif
  ##
  return(obs)
}

## ----draw-five-obs-------------------------------------------------------
(five_obs <- draw_from_experiment(5))

## ----DAG-----------------------------------------------------------------
## plot the causal diagram

## ----approx-psi-zero-a---------------------------------------------------
B <- 1e6 ## 1e6 eventually, or more
full_obs <- draw_from_experiment(B, full = TRUE)
(psi_hat <- mean(full_obs[, "Yone"] - full_obs[, "Yzero"]))

## ----approx-psi-zero-b---------------------------------------------------
sd_hat <- sd(full_obs[, "Yone"] - full_obs[, "Yzero"])
alpha <- 0.05
(psi_CI <- psi_hat + c(-1, 1) * qnorm(1 - alpha / 2) * sd_hat / sqrt(B))

## ----another-simulation--------------------------------------------------
draw_from_another_experiment <- function(n, h = 0) {
  ## preliminary
  n <- Arguments$getInteger(n, c(1, Inf))
  h <- Arguments$getNumeric(h)
  ## ## 'Gbar' and 'Qbar' factors
  Gbar <- function(W) {
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
  W <- runif(n, min = 1/10, max = 9/10)
  ## ## action undertaken
  A <- rbinom(n, size = 1, prob = Gbar(W))
  ## ## reward
  shape1 <- 4
  QAW <- Qbar(cbind(A, W))
  Y <- rbeta(n, shape1 = shape1, shape2 = shape1 * (1 - QAW) / QAW)
  ## ## observation
  obs <- cbind(W = W, A = A, Y = Y)
  attr(obs, "Gbar") <- Gbar
  attr(obs, "Qbar") <- Qbar
  attr(obs, "QW") <- function(x){dunif(x, min = 1/10, max = 9/10)}
  attr(obs, "shape1") <- shape1
  ##
  return(obs)
}

## ----approx-psi-one------------------------------------------------------
five_obs_from_another_experiment <- draw_from_another_experiment(5)
integrand <- function(w) {
  Qbar <- attr(five_obs_from_another_experiment, "Qbar")
  QW <- attr(five_obs_from_another_experiment, "QW")
  ( Qbar(cbind(1, w)) - Qbar(cbind(0, w)) ) * QW(w)
}
(psi_Pi_zero <- integrate(integrand, lower = 0, upper = 1)$val)

## ----psi-approx-psi-one,  fig.cap =  "Evolution of statistical parameter $\\Psi$ along fluctuation $\\{\\Pi_{h} : h \\in H\\}$."----
approx <- seq(-1, 1, length.out = 1e2)
psi_Pi_h <- sapply(approx, function(t) {
  obs_from_another_experiment <- draw_from_another_experiment(1, h = t)
  integrand <- function(w) {
    Qbar <- attr(obs_from_another_experiment, "Qbar")
    QW <- attr(obs_from_another_experiment, "QW")
    ( Qbar(cbind(1, w)) - Qbar(cbind(0, w)) ) * QW(w)
  }
  integrate(integrand, lower = 0, upper = 1)$val  
})
slope_approx <- (psi_Pi_h - psi_Pi_zero) / approx
slope_approx <- slope_approx[min(which(approx > 0))]
ggplot() +
  geom_point(data = data.frame(x = approx, y = psi_Pi_h), aes(x, y),
             color = "#CC6666") +
  geom_segment(aes(x = -1, y = psi_Pi_zero - slope_approx,
                   xend = 1, yend = psi_Pi_zero + slope_approx),
               arrow = arrow(length = unit(0.03, "npc")),
               color = "#9999CC") +
  geom_vline(xintercept = 0, color = "#66CC99") +
  geom_hline(yintercept = psi_Pi_zero, color = "#66CC99") +
  labs(x = "h", y = expression(Psi(Pi[h]))) 

## ----eic-----------------------------------------------------------------
eic <- function(obs, psi) {
  Qbar <- attr(obs, "Qbar")
  Gbar <- attr(obs, "Gbar")
  QAW <- Qbar(obs[, c("A", "W")])
  gW <- Gbar(obs[, "W"])
  lgAW <- obs[, "A"] * gW + (1 - obs[, "A"]) * (1 - gW)
  ( Qbar(cbind(1, obs[, "W"])) - Qbar(cbind(0, obs[, "W"])) - psi ) +
    (2 * obs[, "A"] - 1) / lgAW * (obs[, "Y"] - QAW)
}

(eic(five_obs, psi = psi_hat))
(eic(five_obs_from_another_experiment, psi = psi_Pi_zero))

## ----cramer-rao----------------------------------------------------------
obs <- draw_from_experiment(B)
(cramer_rao_hat <- var(eic(obs, psi = psi_hat)))

## ----cramer-rao-another-experiment---------------------------------------
obs_from_another_experiment <- draw_from_another_experiment(B)
(cramer_rao_Pi_zero_hat <- var(eic(obs_from_another_experiment, psi = 59/300)))
(ratio <- sqrt(cramer_rao_Pi_zero_hat/cramer_rao_hat))

## ----recover-slope-------------------------------------------------------
sigma0_draw_from_another_experiment <- function(obs) { 
  ## preliminary
  Qbar <- attr(obs, "Qbar")
  QAW <- Qbar(obs[, c("A", "W")])
  shape1 <- Arguments$getInteger(attr(obs, "shape1"), c(1, Inf))
  ## computations
  betaAW <- shape1 * (1 - QAW) / QAW
  out <- log(1 - obs[, "Y"])
  for (int in 1:shape1) {
    out <- out + 1/(int - 1 + betaAW)
  }
  out <- - out * shape1 * (1 - QAW) / QAW * 10 * sqrt(obs[, "W"]) * obs[, "A"]
  ## no need to center given how we will use it
  return(out)
}

vars <- eic(obs_from_another_experiment, psi = 59/300) *
  sigma0_draw_from_another_experiment(obs_from_another_experiment)
sd_hat <- sd(vars)
(slope_hat <- mean(vars))
(slope_CI <- slope_hat + c(-1, 1) * qnorm(1 - alpha / 2) * sd_hat / sqrt(B))

## ----known-Gbar-one-a----------------------------------------------------
Gbar <- attr(obs, "Gbar")

iter <- 1e3

## ----known-Gbar-one-b, fig.cap = "Kernel density estimators of the law of two estimators of $\\psi_{0}$, one of them misconceived (a), the other assuming that $\\Gbar_{0}$ is known (b). Built based on `iter` independent realizations of each estimator."----
psi_hat_ab <- obs %>% as_tibble() %>% mutate(id = 1:nrow(obs) %% iter) %>%
  mutate(lgAW = A * Gbar(W) + (1 - A) * (1 - Gbar(W))) %>% group_by(id) %>%
  summarize(est_a = mean(Y[A==1]) - mean(Y[A==0]),
            est_b = mean(Y * (2 * A - 1) / lgAW),
            std_b = sd(Y * (2 * A - 1) / lgAW),
            clt_b = sqrt(n()) * (est_b - psi_hat) / std_b)
std_a <- sd(psi_hat_ab$est_a)
psi_hat_ab <- psi_hat_ab %>%
  mutate(std_a = std_a,
         clt_a = (est_a - psi_hat) / std_a) %>% 
  gather(key, value, -id) %>%
  extract(key, c("what", "type"), "([^_]+)_([ab])") %>%
  spread(what, value)

(bias_ab <- psi_hat_ab %>% group_by(type) %>% summarise(bias = mean(clt)))

fig <- ggplot() +
  geom_line(aes(x = x, y = y), 
            data = tibble(x = seq(-3, 3, length.out = 1e3),
                          y = dnorm(x)),
            linetype = 1, alpha = 0.5) +
  geom_density(aes(clt, fill = type, colour = type),
               psi_hat_ab, alpha = 0.1) +
  geom_vline(aes(xintercept = bias, colour = type),
             bias_ab, size = 1.5, alpha = 0.5)
  
fig +
  labs(x = expression(paste(sqrt(n)*(psi[n]^{list(a, b)} - psi[0]))))

## ----unknown-Gbar-one----------------------------------------------------
estimate_G <- function(dat, working_model) {
  fit <- working_model[[1]](formula = working_model[[2]], data = dat)
  Ghat <- function(newdata) {
    predict(fit, newdata, type = "response")
  }
  return(Ghat)
}

predict_lGAW <- function(A, W, working_model, threshold = 0.05) {
  ## fit the working model
  dat <- data.frame(A = A, W= W)
  Ghat <- estimate_G(dat, working_model)
  ## make predictions based on the fit
  Ghat_W <- Ghat(dat)
  A <- dat[, "A"]
  lGAW <- A * Ghat_W + (1 - A) * (1 - Ghat_W)
  pmin(1 - threshold, pmax(lGAW, threshold))
}

working_model <- list(
  model = function(...) {glm(family = binomial(), ...)},
  formula = as.formula(
    paste("A ~",
          paste("I(W^", seq(1/2, 3, by = 1/2), sep = "", collapse = ") + "),
          ")")
  ))
working_model$formula

## ----unknown-Gbar-two, fig.cap  = "Kernel density estimators of the law of three estimators of $\\psi_{0}$, one of them misconceived (a), one assuming that $\\Gbar_{0}$ is known (b) and one that hinges on the estimation of $\\Gbar_{0}$ (c). The present figure includes Figure \\@ref(fig:known-Gbar-one-b) (but the colors differ). Built based on `iter` independent realizations of each estimator."----
psi_hat_c <- obs %>% as_tibble() %>% mutate(id = 1:nrow(obs) %% iter) %>%
  group_by(id) %>%
  mutate(lgAW = predict_lGAW(A, W, working_model)) %>%
  summarize(est = mean(Y * (2 * A - 1) / lgAW))
std_c <- sd(psi_hat_c$est)
psi_hat_abc <- psi_hat_c %>%
  mutate(std = std_c,
         clt = (est - psi_hat) / std,
         type = "c") %>%
  full_join(psi_hat_ab)

bias_abc <- psi_hat_abc %>% group_by(type) %>% summarise(bias = mean(clt))

fig +
  geom_density(aes(clt, fill = type, colour = type), psi_hat_abc, alpha = 0.1) +
  geom_vline(aes(xintercept = bias, colour = type),
             bias_abc, size = 1.5, alpha = 0.5) +
  xlim(-3, 3) + 
  labs(x = expression(paste(sqrt(n)*(psi[n]^{list(a, b, c)} - psi[0]))))

## ----unknown-Gbar-three, fig.cap  = "Quantile-quantile plot of the standard normal law against the empirical laws  of three estimators of $\\psi_{0}$, one of them misconceived (a), one assuming that $\\Gbar_{0}$ is known (b) and one that hinges on the estimation of $\\Gbar_{0}$ (c). Built based on `iter` independent realizations of each estimator."----
ggplot(psi_hat_abc, aes(sample = clt, fill = type, colour = type)) +
  geom_abline(intercept = 0, slope = 1, alpha = 0.5) +
  geom_qq(alpha = 1)

