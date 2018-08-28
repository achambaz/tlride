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

## ----redo----------------------------------------------------------------
redo_fixed <- c(TRUE, FALSE)[2]
redo_varying <- c(TRUE, FALSE)[2]
## if  'redo_$'  then  recompute  'learned_features_$_sample_size',  otherwise
## upload it if it is not already in the environment.
if (!redo_fixed) {
  if (!exists("learned_features_fixed_sample_size")) {
    learned_features_fixed_sample_size <-
      loadObject("data/learned_features_fixed_sample_size_new.xdr")
  }
}
if (!redo_varying) {
  if (!exists("learned_features_varying_sample_size")) {
    learned_features_varying_sample_size <-
      loadObject("data/learned_features_varying_sample_size_new.xdr")
  }
} 

## ----visible-setup-------------------------------------------------------
set.seed(54321) ## because reproducibility matters...
suppressMessages(library(R.utils)) ## make sure it is installed
suppressMessages(library(tidyverse)) ## make sure it is installed
suppressMessages(library(caret)) ## make sure it is installed
suppressMessages(library(ggdag)) ## make sure it is installed
expit <- plogis
logit <- qlogis

## ANTOINE: hide the four methods?
setMethodS3("print", "law", function(this, ...) {
  cat("An experiment. Use method 'run' to run it.\n")
})

setMethodS3("run", "law", function(this, n, ...) {
  n <- Arguments$getInteger(n, c(1, Inf))
  this$.run_experiment(n, ...)
})

## ## 'uncover', because I'm looking for a **verb**
## ## (started with 'oracle' then 'view_as_oracle')
## ## possible alternatives: 'grasp', 'discern'
setMethodS3("uncover", "law", function(this, ...) {
  this$.some_relevant_features
})

## ## 'evaluate_psi', because I'm looking for a **verb**
## ## (started with 'Psi')
## ##
## ## if made visible, may define later than 'print',
## ## 'run', and 'uncover' 
setMethodS3("evaluate_psi", "law", function(this, ...) {
  some_relevant_features <- uncover(this)
  ellipsis <- list(...)
  if (length(intersect(names(some_relevant_features), c("Qbar", "QW"))) != 2) {
    throw("Is law '", deparse(substitute(this)),
          "' an element of the model where 'Psi' is defined?")
  } else {
    Qbar <- some_relevant_features$Qbar
    if (length(formals(Qbar)) > 1) {
      params_Qbar <- formals(Qbar)[-1]
      params_Qbar <- params_Qbar[sapply(params_Qbar, is.symbol)]
      if (length(params_Qbar) > 0) {
        if (!all(names(params_Qbar) %in% names(ellipsis))) {
          throw("Is law '", deparse(substitute(this)),
                "' fully characterized?")
        } else {
          idx <- which(names(ellipsis) %in% names(params_Qbar))
          Qbar <- function(AW) {
            do.call(some_relevant_features$Qbar,
                    rlist::list.merge(list(AW = AW), ellipsis[idx]))
          }
        }
      }
    }
    QW <- some_relevant_features$QW
    if (length(formals(QW)) > 1) {
      params_QW <- formals(QW)[-1]
      params_QW <- params_QW[sapply(params_QW, is.symbol)]
      if (length(params_QW) > 0) {
        if (!all(names(params_QW) %in% names(ellipsis))) {
          throw("Is law '", deparse(substitute(this)),
                "' fully characterized?")
        } else {
          idx <- which(names(ellipsis) %in% names(params_QW))
          QW <- function(W) {
            do.call(some_relevant_features$QW,
                    rlist::list.merge(list(W = W), ellipsis[idx]))
          }
        }
      }
    }
    integrand <- function(w) {
      ( Qbar(cbind(1, w)) - Qbar(cbind(0, w)) ) * QW(w)
    }
    out <- integrate(integrand, lower = 0, upper = 1)$val
    return(out)
  }
})


## ANTOINE: hide this too?
sample_from_mixture_of_uniforms <-
  function(n, mixture_weights, mins, maxs) {
    ## preliminary
    n <- Arguments$getInteger(n, c(1, Inf))
    mixture_weights <- Arguments$getNumerics(mixture_weights, c(0, Inf))
    mins <- Arguments$getNumerics(mins)
    maxs <- Arguments$getNumerics(maxs)
    if (!(length(mixture_weights) == length(mins) &
          length(mins) == length(maxs))) {
      throw(paste0("Arguments 'mixture_weights', 'mins' and 'maxs' ",
                   "must have the same length."))
    }
    if (!sum(mixture_weights) == 1) {
      throw("The entries of 'mixture_weights' must sum up to one.")
    }
    if (!all(mins <= maxs)) {
      throw("Caution: 'mins[i]' must be smaller than 'maxs[i]' for all 'i'.")
    }
    ##
    latent <- findInterval(runif(n), cumsum(mixture_weights)) + 1
    W <- runif(n, min = mins[latent], max = maxs[latent])
    return(W)
  }

## ----simulation----------------------------------------------------------

## ANTOINE: hide also the definition of the experiment, at least temporarily?
experiment <- list(
  .some_relevant_features = list(
    Gbar = function(W) {
      expit(1 + 2 * W - 4 * sqrt(abs((W - 5/12))))
    },
    Qbar =  function(AW) {
      A <- AW[, 1]
      W <- AW[, 2]
      A * (cos((-1/2 + W) * pi) * 2/5 + 1/5 + (1/3 <= W & W <= 1/2) / 5 +
           (W >= 3/4) * (W - 3/4) * 2) +
        (1 - A) * (sin(4 * W^2 * pi) / 4 + 1/2) 
    },
    QW = function(W,
                  mixture_weights = c(1/10, 9/10, 0),
                  mins = c(0, 11/30, 0),
                  maxs = c(1, 14/30, 1)) {
      out <- sapply(1:length(mixture_weights),
                    function(ii){
                      mixture_weights[ii] *
                        dunif(W, min = mins[ii], max = maxs[ii])
                    })
      return(rowSums(out))
    },
    qY = function(AW, Y, Qbar){
      A <- AW[, 1]
      W <- AW[, 2]
      Qbar_AW <- do.call(Qbar, list(AW)) # is call to 'do.call' necessary?
      shape1 <- ifelse(A == 0, 2, 3)
      dbeta(Y, shape1 = shape1, shape2 = shape1 * (1 - Qbar_AW) / Qbar_AW)
    }
  )  
)
class(experiment) <- c(class(experiment), "law")
experiment$.run_experiment <- function(n, ideal = FALSE) {
  ## preliminary
  n <- Arguments$getInteger(n, c(1, Inf))
  ideal <- Arguments$getLogical(ideal)
  ## ## 'Gbar' and 'Qbar' factors
  Gbar <- experiment$.some_relevant_features$Gbar
  Qbar <- experiment$.some_relevant_features$Qbar
  ## sampling
  ## ## context
  params <- formals(experiment$.some_relevant_features$QW)
  mixture_weights <- eval(params$mixture_weights)
  mins <- eval(params$mins)
  maxs <- eval(params$maxs)
  W <- sample_from_mixture_of_uniforms(n, mixture_weights,
                                       mins, maxs)
  ## ## counterfactual rewards
  zeroW <- cbind(A = 0, W)
  oneW <- cbind(A = 1, W)
  Qbar_zeroW <- Qbar(zeroW)
  Qbar_oneW <- Qbar(oneW)
  Yzero <- rbeta(n, shape1 = 2, shape2 = 2 * (1 - Qbar_zeroW) / Qbar_zeroW)
  Yone <- rbeta(n, shape1 = 3, shape2 = 3 * (1 - Qbar_oneW) / Qbar_oneW)
  ## ## action undertaken
  A <- rbinom(n, size = 1, prob = Gbar(W))
  ## ## actual reward
  Y <- A * Yone + (1 - A) * Yzero
  ## ## observation
  if (ideal) {
    obs <- cbind(W = W, Yzero = Yzero, Yone = Yone, A = A, Y = Y)
  } else {
    obs <- cbind(W = W, A = A, Y = Y)
  }
  return(obs)
}


## ----draw-five-obs-------------------------------------------------------

## ANTOINE: new version
(five_obs <- run(experiment, 5))

## ----exercise:visualize, eval = TRUE-------------------------------------

## ANTOINE: new version
some_relevant_features <- uncover(experiment)
Gbar <- some_relevant_features$Gbar
Qbar <- some_relevant_features$Qbar
QW <- some_relevant_features$QW

features <- tibble(w = seq(0, 1, length.out = 1e3)) %>%
  mutate(Qw = QW(w),
         Gw = Gbar(w),
         Q1w = Qbar(cbind(A = 1, W = w)),
         Q0w = Qbar(cbind(A = 0, W = w)),
         blip_Qw = Q1w - Q0w)

features %>% select(-Qw, -Gw) %>%
  rename("Q(1,.)" = Q1w,
         "Q(0,.)" = Q0w,
         "Q(1,.) - Q(0,.)" = blip_Qw) %>%
  gather("f", "value", -w) %>%
  ggplot() +
  geom_line(aes(x = w, y = value, color = f), size = 1) +
  labs(y = "f(w)", title = bquote("Visualizing" ~ bar(Q)[0])) +
  ylim(NA, 1)

## ----approx-psi-0-a-one--------------------------------------------------

## ANTOINE: new version
(psi_zero <- evaluate_psi(experiment))

## ----DAG, out.width = '70%', fig.align = 'center', fig.width = 8, fig.height = 6, fig.cap = "Causal graph summarizing the inner causal mechanism at play in `run\\_experiment`."----
dagify(
  Y ~ A + Y1 + Y0, A ~ W, Y1 ~ W, Y0 ~ W,
  labels = c(Y = "Actual reward",
             A = "Action",
             Y1 = "Counterfactual reward\n of action 1",
             Y0 = "Counterfactual reward\n of action 0",
             W = "Context of action"),
  coords = list(
    x = c(W = 0, A = -1, Y1 = 1.5, Y0 = 0.25, Y = 1),
    y = c(W = 0, A = -1, Y1 = -0.5, Y0 = -0.5, Y = -1)),
  outcome = "Y",
  exposure = "A",
  latent = c("Y0", "Y1")) %>% tidy_dagitty %>%
  ggdag(text = TRUE, use_labels = "label") + theme_dag_grey()

## ----approx-psi-zero-a-two-----------------------------------------------
B <- 1e6
ideal_obs <- run(experiment, B, ideal = TRUE)
(psi_approx <- mean(ideal_obs[, "Yone"] - ideal_obs[, "Yzero"]))

## ----approx-psi-zero-b---------------------------------------------------
sd_approx <- sd(ideal_obs[, "Yone"] - ideal_obs[, "Yzero"])
alpha <- 0.05
(psi_approx_CI <- psi_approx + c(-1, 1) * qnorm(1 - alpha / 2) * sd_approx / sqrt(B))

## ----another-simulation--------------------------------------------------

## ANTOINE: new version
another_experiment <- list(
  .some_relevant_features = list(
    Gbar = function(W) {
      sin((1 + W) * pi / 6)
    },
    Qbar =   function(AW, h) {
      A <- AW[, 1]
      W <- AW[, 2]
      expit( logit( A *  W + (1 - A) * W^2 ) +
             h * 10 * sqrt(W) * A )
    },
    QW = function(x, min = 1/10, max = 9/10){dunif(x, min = min, max = max)},
    qY = function(AW, Y, Qbar, shape1 = 4){
      A <- AW[, 1]
      W <- AW[, 2]
      Qbar_AW <- do.call(Qbar, list(AW))
      dbeta(Y, shape1 = shape1, shape2 = shape1 * (1 - Qbar_AW) / Qbar_AW)
    }
  )  
)
class(another_experiment) <- c(class(another_experiment), "law")
another_experiment$.run_experiment <- function(n, h) {
  ## preliminary
  n <- Arguments$getInteger(n, c(1, Inf))
  h <- Arguments$getNumeric(h)
  ## ## 'Gbar' and 'Qbar' factors
  Gbar <- another_experiment$.some_relevant_features$Gbar
  Qbar <- another_experiment$.some_relevant_features$Qbar
  ## sampling
  ## ## context
  params <- formals(another_experiment$.some_relevant_features$QW)
  W <- runif(n, min = eval(params$min),
             max = eval(params$max))
  ## ## action undertaken
  A <- rbinom(n, size = 1, prob = Gbar(W))
  ## ## reward
  params <- formals(another_experiment$.some_relevant_features$qY)
  shape1 <- eval(params$shape1)
  Qbar_AW <- Qbar(cbind(A, W), h = h)
  Y <- rbeta(n, shape1 = shape1, shape2 = shape1 * (1 - Qbar_AW) / Qbar_AW)
  ## ## observation
  obs <- cbind(W = W, A = A, Y = Y)
  return(obs)
}

## ----approx-psi-one------------------------------------------------------

## ANTOINE: new version
(five_obs_from_another_experiment <- run(another_experiment, 5, h = 0))
(psi_Pi_zero <- evaluate_psi(another_experiment, h = 0))



## ----psi-approx-psi-one,  fig.cap =  "Evolution of statistical parameter $\\Psi$ along fluctuation $\\{\\Pi_{h} : h \\in H\\}$."----

## ANTOINE: new version
approx <- seq(-1, 1, length.out = 1e2)
psi_Pi_h <- sapply(approx, function(t) {
  evaluate_psi(another_experiment, h = t)
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

stop("Stopping here...\n")

## ----eic-----------------------------------------------------------------
eic <- function(obs, psi) {
  Qbar <- attr(obs, "Qbar")
  Gbar <- attr(obs, "Gbar")
  QAW <- Qbar(obs[, c("A", "W")])
  QoneW <- Qbar(cbind(A = 1, W = obs[, "W"]))
  QzeroW <- Qbar(cbind(A = 0, W = obs[, "W"]))
  GW <- Gbar(obs[, "W", drop = FALSE])
  lGAW <- obs[, "A"] * GW + (1 - obs[, "A"]) * (1 - GW)
  out <- (QoneW - QzeroW - psi) + (2 * obs[, "A"] - 1) / lGAW * (obs[, "Y"] - QAW)
  out <- as.vector(out)
  return(out)
}

(eic(five_obs, psi = psi_approx))
(eic(five_obs_from_another_experiment, psi = psi_Pi_zero))

## ----cramer-rao----------------------------------------------------------
obs <- run_experiment(B)
(cramer_rao_hat <- var(eic(obs, psi = psi_approx)))

## ----cramer-rao-another-experiment---------------------------------------
obs_from_another_experiment <- run_another_experiment(B)
(cramer_rao_Pi_zero_hat <- var(eic(obs_from_another_experiment, psi = 59/300)))
(ratio <- sqrt(cramer_rao_Pi_zero_hat/cramer_rao_hat))

## ----recover-slope-------------------------------------------------------
sigma0_run_another_experiment <- function(obs) { 
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
  sigma0_run_another_experiment(obs_from_another_experiment)
sd_hat <- sd(vars)
(slope_hat <- mean(vars))
(slope_CI <- slope_hat + c(-1, 1) * qnorm(1 - alpha / 2) * sd_hat / sqrt(B))

## ----known-Gbar-one-a----------------------------------------------------
Gbar <- attr(obs, "Gbar")

iter <- 1e3

## ----known-Gbar-one-b, fig.cap = "Kernel density estimators of the law of two estimators of $\\psi_{0}$ (recentered with respect to $\\psi_{0}$, and renormalized), one of them misconceived (a), the other assuming that $\\Gbar_{0}$ is known (b). Built based on `iter` independent realizations of each estimator."----
psi_hat_ab <- obs %>% as_tibble() %>%
  mutate(id = (seq_len(n()) - 1) %% iter) %>%
  mutate(lGAW = A * Gbar(W) + (1 - A) * (1 - Gbar(W))) %>% group_by(id) %>%
  summarize(est_a = mean(Y[A==1]) - mean(Y[A==0]),
            est_b = mean(Y * (2 * A - 1) / lGAW),
            std_b = sd(Y * (2 * A - 1) / lGAW) / sqrt(n()),
            clt_b = (est_b - psi_approx) / std_b) %>% 
  mutate(std_a = sd(est_a),
         clt_a = (est_a - psi_approx) / std_a) %>%
  gather("key", "value", -id) %>%
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
  labs(y = "",
       x = expression(paste(sqrt(n/v[n]^{list(a, b)})*(psi[n]^{list(a, b)} - psi[0]))))

## ----unknown-Gbar-one----------------------------------------------------
estimate_G <- function(dat, algorithm, ...) {
  if (!is.data.frame(dat)) {
    dat <- as.data.frame(dat)
  }
  if (!attr(algorithm, "ML")) {
    fit <- algorithm[[1]](formula = algorithm[[2]], data = dat)
  } else {
    fit <- algorithm[[1]](dat, ...)
  }
  fit$type_of_preds <- algorithm$type_of_preds
  return(fit)
}

compute_lGhatAW <- function(A, W, Ghat, threshold = 0.05) {
  dat <- data.frame(A = A, W = W)
  Ghat_W <- predict(Ghat, newdata = dat, type = Ghat$type_of_preds)
  lGAW <- A * Ghat_W + (1 - A) * (1 - Ghat_W)
  pred <- pmin(1 - threshold, pmax(lGAW, threshold))
  return(pred)
}

## ----unknown-Gbar-two----------------------------------------------------
trim_glm_fit <- caret::getModelInfo("glm")$glm$trim
working_model_G_one <- list(
  model = function(...) {trim_glm_fit(glm(family = binomial(), ...))},
  formula = as.formula(
    paste("A ~",
          paste(c("I(W^", "I(abs(W - 5/12)^"),
                rep(seq(1/2, 3/2, by = 1/2), each = 2),
                sep = "", collapse = ") + "),
          ")")
  ),
  type_of_preds = "response"
)
attr(working_model_G_one, "ML") <- FALSE
working_model_G_one$formula

## ----unknown-Gbar-two-bis------------------------------------------------
if (redo_fixed) {
  learned_features_fixed_sample_size <-
    obs %>% as_tibble() %>%
    mutate(id = (seq_len(n()) - 1) %% iter) %>%
    nest(-id, .key = "obs") %>%
    mutate(Ghat = map(obs, ~ estimate_G(., algorithm = working_model_G_one))) %>%
    mutate(lGAW = map2(Ghat, obs, ~ compute_lGhatAW(.y$A, .y$W, .x)))
}

psi_hat_abc <-
  learned_features_fixed_sample_size %>%
  unnest(obs, lGAW) %>%
  group_by(id) %>%
  summarize(est = mean(Y * (2 * A - 1) / lGAW)) %>%
  mutate(std = sd(est),
         clt = (est - psi_approx) / std,
         type = "c") %>%
  full_join(psi_hat_ab)

(bias_abc <- psi_hat_abc %>% group_by(type) %>% summarise(bias = mean(clt)))

## ----unknown-Gbar-three, fig.cap = "Kernel density estimators of the law of three estimators of $\\psi_{0}$  (recentered with respect to $\\psi_{0}$, and renormalized), one of them misconceived (a), one assuming that $\\Gbar_{0}$ is known (b) and one that hinges on the estimation of $\\Gbar_{0}$ (c). The present figure includes Figure \\@ref(fig:known-Gbar-one-b) (but the colors differ). Built based on `iter` independent realizations of each estimator."----
fig +
  geom_density(aes(clt, fill = type, colour = type), psi_hat_abc, alpha = 0.1) +
  geom_vline(aes(xintercept = bias, colour = type),
             bias_abc, size = 1.5, alpha = 0.5) +
  xlim(-3, 4) + 
  labs(y = "",
       x = expression(paste(sqrt(n/v[n]^{list(a, b, c)})*
                            (psi[n]^{list(a, b, c)} - psi[0]))))

## ----unknown-Gbar-four, fig.cap  = "Quantile-quantile plot of the standard normal law against the empirical laws  of three estimators of $\\psi_{0}$, one of them misconceived (a), one assuming that $\\Gbar_{0}$ is known (b) and one that hinges on the estimation of $\\Gbar_{0}$ (c). Built based on `iter` independent realizations of each estimator."----
ggplot(psi_hat_abc, aes(sample = clt, fill = type, colour = type)) +
  geom_abline(intercept = 0, slope = 1, alpha = 0.5) +
  geom_qq(alpha = 1)

## ----exercises-one, eval = FALSE-----------------------------------------
## powers <- ## make sure '1/2' and '1' belong to 'powers', eg
##   seq(1/4, 3, by = 1/4)
## working_model_G_two <- list(
##   model = function(...) {trim_glm_fit(glm(family = binomial(), ...))},
##   formula = as.formula(
##     paste("A ~",
##           paste(c("I(W^", "I(abs(W - 5/12)^"),
##                 rep(powers, each = 2),
##                 sep = "", collapse = ") + "),
##           ")")
##   ),
##   type_of_preds = "response"
## )
## attr(working_model_G_two, "ML") <- FALSE

## ----exercises-two, eval = TRUE------------------------------------------
transform <- c("cos", "sin", "sqrt", "log", "exp")
working_model_G_three <- list(
  model = function(...) {trim_glm_fit(glm(family = binomial(), ...))},
  formula = as.formula(
    paste("A ~",
          paste("I(", transform, sep = "", collapse = "(W)) + "),
          "(W))")
  ),
  type_of_preds = "response"
)
attr(working_model_G_three, "ML") <- FALSE
(working_model_G_three$formula)

## ----estimating-Qbar-one-------------------------------------------------
estimate_Q <- function(dat, algorithm, ...) {
  if (!is.data.frame(dat)) {
    dat <- as.data.frame(dat)
  }
  if (!attr(algorithm, "ML")) {
    fit <- algorithm[[1]](formula = algorithm[[2]], data = dat)
  } else {
    fit <- algorithm[[1]](dat, ...)
  }
  fit$type_of_preds <- algorithm$type_of_preds
  return(fit)
}

compute_QhatAW <- function(Y, A, W, Qhat, blip = FALSE) {
  if (!blip) {
    dat <- data.frame(Y = Y, A = A, W = W)
    pred <- predict(Qhat, newdata = dat, type = Qhat$type_of_preds)
  } else {
    pred <- predict(Qhat, newdata = data.frame(A = 1, W = W),
                    type = Qhat$type_of_preds) -
      predict(Qhat, newdata = data.frame(A = 0, W = W),
              type = Qhat$type_of_preds)
  }
  return(pred)  
}

working_model_Q_one <- list(
  model = function(...) {trim_glm_fit(glm(family = binomial(), ...))},
  formula = as.formula(
    paste("Y ~ A * (",
          paste("I(W^", seq(1/2, 3/2, by = 1/2), sep = "", collapse = ") + "),
          "))")
  ),
  type_of_preds = "response"
)
attr(working_model_Q_one, "ML") <- FALSE
working_model_Q_one$formula

## k-NN
kknn_algo <- list(
  algo = function(dat, ...) {
    args <- list(...)
    if ("Subsample" %in% names(args)) {
      keep <- sample.int(nrow(dat), args$Subsample)
      dat <- dat[keep, ]
    }
    fit <- caret::train(Y ~ I(10*A) + W, ## a tweak
                        data = dat,
                        method = "kknn",
                        verbose = FALSE,
                        ...)
    fit$finalModel$fitted.values <- NULL
    ## nms <- names(fit$finalModel$data)
    ## for (ii in match(setdiff(nms, ".outcome"), nms)) {
    ##   fit$finalModel$data[[ii]] <- NULL
    ## }
    fit$trainingData <- NULL    
    return(fit)
  },
  type_of_preds = "raw"
)
attr(kknn_algo, "ML") <- TRUE
kknn_grid <- expand.grid(kmax = 5, distance = 2, kernel = "gaussian")
control <- trainControl(method = "cv", number = 2,
                        predictionBounds = c(0, 1),
                        trim = TRUE,
                        allowParallel = TRUE)

## ----estimating-Qbar-one-bis, fig.cap = "Write caption."-----------------
if(redo_fixed) {
  learned_features_fixed_sample_size <-
    learned_features_fixed_sample_size %>% # head(n = 100) %>%
    mutate(Qhat_d = map(obs, ~ estimate_Q(., algorithm = working_model_Q_one)),
           Qhat_e = map(obs, ~ estimate_Q(., algorithm = kknn_algo,
                                          trControl = control,
                                          tuneGrid = kknn_grid))) %>%
    mutate(blip_QW_d = map2(Qhat_d, obs,
                            ~ compute_QhatAW(.y$Y, .y$A, .y$W, .x, blip = TRUE)),
           blip_QW_e = map2(Qhat_e, obs,
                            ~ compute_QhatAW(.y$Y, .y$A, .y$W, .x, blip = TRUE)))
}

psi_hat_de <- learned_features_fixed_sample_size %>%
  unnest(blip_QW_d, blip_QW_e) %>%
  group_by(id) %>%
  summarize(est_d = mean(blip_QW_d),
            est_e = mean(blip_QW_e)) %>%
  mutate(std_d = sd(est_d),
         std_e = sd(est_e),
         clt_d = (est_d - psi_approx) / std_d,
         clt_e = (est_e - psi_approx) / std_e) %>% 
  gather("key", "value", -id) %>%
  extract(key, c("what", "type"), "([^_]+)_([de])") %>%
  spread(what, value)

(bias_de <- psi_hat_de %>% group_by(type) %>% summarize(bias = mean(clt)))

fig <- ggplot() +
  geom_line(aes(x = x, y = y), 
            data = tibble(x = seq(-3, 3, length.out = 1e3),
                          y = dnorm(x)),
            linetype = 1, alpha = 0.5) +
  geom_density(aes(clt, fill = type, colour = type),
               psi_hat_de, alpha = 0.1) +
  geom_vline(aes(xintercept = bias, colour = type),
             bias_de, size = 1.5, alpha = 0.5)
  
fig +
  labs(y = "",
       x = expression(paste(sqrt(n/v[n]^{list(d, e)})*(psi[n]^{list(d, e)} - psi[0]))))

## ----estimating-Qbar-two, eval = TRUE------------------------------------
sample_size <- c(4e3, 9e3)
block_size <- sum(sample_size)

label <- function(xx, sample_size = c(1e3, 2e3)) {
  by <- sum(sample_size)
  xx <- xx[seq_len((length(xx) %/% by) * by)] - 1
  prefix <- xx %/% by
  suffix <- findInterval(xx %% by, cumsum(sample_size))
  paste(prefix + 1, suffix + 1, sep = "_")
}

if (redo_varying) {
  learned_features_varying_sample_size <- obs %>% as.tibble %>% 
    head(n = (nrow(.) %/% block_size) * block_size) %>% 
    mutate(block = label(1:nrow(.), sample_size)) %>%
    nest(-block, .key = "obs")
} 

## ----estimating-Qbar-three, eval = TRUE----------------------------------
if(redo_varying) {
  learned_features_varying_sample_size <-
    learned_features_varying_sample_size %>% 
    mutate(Qhat_d = map(obs, ~ estimate_Q(., algorithm = working_model_Q_one)),
           Qhat_e = map(obs, ~ estimate_Q(., algorithm = kknn_algo,
                                          trControl = control,
                                          tuneGrid = kknn_grid))) %>%
    mutate(blip_QW_d = map2(Qhat_d, obs,
                            ~ compute_QhatAW(.y$Y, .y$A, .y$W, .x, blip = TRUE)),
           blip_QW_e = map2(Qhat_e, obs,
                            ~ compute_QhatAW(.y$Y, .y$A, .y$W, .x, blip = TRUE)))
}

root_n_bias <- learned_features_varying_sample_size %>%
  unnest(blip_QW_d, blip_QW_e) %>%
  group_by(block) %>%
  summarize(clt_d = sqrt(n()) * (mean(blip_QW_d) - psi_approx),
            clt_e = sqrt(n()) * (mean(blip_QW_e) - psi_approx)) %>%
  gather("key", "value", -block) %>%
  extract(key, c("what", "type"), "([^_]+)_([de])") %>%
  spread(what, value) %>%
  mutate(block = unlist(map(strsplit(block, "_"), ~.x[2])),
         sample_size = sample_size[as.integer(block)])

## ----estimating-Qbar-four, fig.width = 5, fig.height = 5, fig.cap  = "Evolution of root-$n$ times bias versus sample size for two inference methodology of $\\psi_{0}$ based on the estimation of $\\Qbar_{0}$. Big dots represent the average biases and vertical lines represent twice the standard error."----
root_n_bias <- learned_features_fixed_sample_size %>%
  mutate(sample_size = B/iter) %>%  # because *fixed* sample size
  unnest(blip_QW_d, blip_QW_e) %>%
  group_by(id) %>%
  summarize(clt_d = sqrt(n()) * (mean(blip_QW_d) - psi_approx),
            clt_e = sqrt(n()) * (mean(blip_QW_e) - psi_approx),
            sample_size = sample_size[1]) %>%
  gather("key", "clt", -id, -sample_size) %>%
  extract(key, c("what", "type"), "([^_]+)_([de])") %>%
  mutate(block = "0") %>% select(-id, -what) %>%
  full_join(root_n_bias)

root_n_bias %>%
  ggplot() +
  stat_summary(aes(x = sample_size, y = clt,
                   group = interaction(sample_size, type),
                   color = type),
               fun.data = mean_se, fun.args = list(mult = 2),
               position = position_dodge(width = 250), cex = 1) +
  stat_summary(aes(x = sample_size, y = clt,
                   group = interaction(sample_size, type),
                   color = type),
               fun.data = mean_se, fun.args = list(mult = 2),
               position = position_dodge(width = 250), cex = 1,
               geom = "errorbar", width = 750) +
  stat_summary(aes(x = sample_size, y = clt,
                   color = type),
               fun.y = mean, 
               position = position_dodge(width = 250),
               geom = "polygon", fill = NA) +
  geom_point(aes(x = sample_size, y = clt,
                 group = interaction(sample_size, type),
                 color = type),
             position = position_dodge(width = 250),
             alpha = 0.1) +
  scale_x_continuous(breaks = unique(c(B / iter, sample_size))) +
  labs(x = "sample size n",
       y = expression(paste(sqrt(n) * (psi[n]^{list(d, e)} - psi[0]))))

## execute
## rm(learned_features_fixed_sample_size)
## as soon as possible!

## ----one-step-one--------------------------------------------------------
set_Qbar_Gbar <- function(obs, Qhat, Ghat) {
  attr(obs, "Qbar") <- function(newdata) {
    if (!is.data.frame(newdata)) {
      newdata <- as.data.frame(newdata)
    }
    predict(Qhat, newdata = newdata, type = Qhat$type_of_preds)
  }
  attr(obs, "Gbar") <- function(newdata) {
    if (!is.data.frame(newdata)) {
      newdata <- as.data.frame(newdata)
    }
    predict(Ghat, newdata = newdata, type = Ghat$type_of_preds)
  }
  return(obs)
}
eic_hat <- function(obs, Qhat, Ghat, psi_hat) {
  Qbar <- function(newdata) {
    if (!is.data.frame(newdata)) {
      newdata <- as.data.frame(newdata)
    }
    predict(Qhat, newdata = newdata, type = Qhat$type_of_preds)
  }
  Gbar <- function(newdata) {
    if (!is.data.frame(newdata)) {
      newdata <- as.data.frame(newdata)
    }
    predict(Ghat, newdata = newdata, type = Ghat$type_of_preds)
  }
  QAW <- Qbar(obs[, c("A", "W")])
  QoneW <- Qbar(cbind(A = 1, W = obs[, "W"]))
  QzeroW <- Qbar(cbind(A = 0, W = obs[, "W"]))
  GW <- Gbar(obs[, "W", drop = FALSE])
  lGAW <- obs[, "A"] * GW + (1 - obs[, "A"]) * (1 - GW)
  out <- (QoneW - QzeroW - psi_hat) + (2 * obs[, "A"] - 1) / lGAW * (obs[, "Y"] - QAW)
  out <- out[[1]]
  return(out)
}

## ----one-step-two, fig.cap = "Write caption."----------------------------
psi_hat_de_one_step <- learned_features_fixed_sample_size %>%
  mutate(est_d = map(blip_QW_d, mean),
         est_e = map(blip_QW_e, mean)) %>%
  mutate(eic_obs_d = pmap(list(obs, Qhat_d, Ghat, est_d),
                          eic_hat),
         eic_obs_e = pmap(list(obs, Qhat_e, Ghat, est_e),
                          eic_hat)) %>%
  unnest(blip_QW_d, eic_obs_d,
         blip_QW_e, eic_obs_e) %>%
  group_by(id) %>%
  summarize(est_d = mean(blip_QW_d) + mean(eic_obs_d),
            std_d = sd(eic_obs_d),
            clt_d = sqrt(n()) * (est_d - psi_approx) / std_d,
            est_e = mean(blip_QW_e) + mean(eic_obs_e),
            std_e = sd(eic_obs_e),
            clt_e = sqrt(n()) * (est_e - psi_approx) / std_e) %>%
  gather("key", "value", -id) %>%
  extract(key, c("what", "type"), "([^_]+)_([de])") %>%
  spread(what, value) %>%
  mutate(type = paste0(type, "_one_step"))
  
(bias_de_one_step <- psi_hat_de_one_step %>%
   group_by(type) %>% summarize(bias = mean(clt)))

ggplot() +
  geom_line(aes(x = x, y = y), 
            data = tibble(x = seq(-3, 3, length.out = 1e3),
                          y = dnorm(x)),
            linetype = 1, alpha = 0.5) +
  geom_density(aes(clt, fill = type, colour = type),
               psi_hat_de_one_step, alpha = 0.1) +
  geom_vline(aes(xintercept = bias, colour = type),
             bias_de_one_step, size = 1.5, alpha = 0.5) +  
  labs(y = "",
       x = expression(
         paste(sqrt(n/v[n]^{list(dos, eos)}) * (psi[n]^{list(dos, eos)} - psi[0]))))

## ----remove-soon---------------------------------------------------------
bind_rows(bias_de, bias_de_one_step)

## ----enhance-------------------------------------------------------------
psi_hat_de %>%
  full_join(psi_hat_de_one_step) %>% group_by(type) %>%
  summarize(sd = mean(std * ifelse(str_detect(type, "one_step"), 1, NA),
                      se = sd(est) * sqrt(n()),
                      mse = mean((est - psi_approx)^2) * n()))

## ----estimating-Qbar-appendix, eval = FALSE------------------------------
## 
## 
## working_model_Q_two <- list(
##   model = function(...) {trim_glm_fit(glm(family = binomial(), ...))},
##   formula = as.formula(
##     paste("Y ~ A * (",
##           paste("I(W^", seq(1/2, 3, by = 1/2), sep = "", collapse = ") + "),
##           "))")
##   ),
##   type_of_preds = "response"
## )
## attr(working_model_Q_two, "ML") <- FALSE
## 
## ## xgboost based on trees
## xgb_tree_algo <- list(
##   algo = function(dat, ...) {
##     caret::train(Y ~ I(10*A) + W,
##                  data = dat,
##                  method = "xgbTree",
##                  trControl = control,
##                  tuneGrid = grid,
##                  verbose = FALSE)
##   },
##   type_of_preds = "response"
## )
## attr(xgb_tree_algo, "ML") <- TRUE
## xgb_tree_grid <- expand.grid(nrounds = 350,
##                              max_depth = c(4, 6),
##                              eta = c(0.05, 0.1),
##                              gamma = 0.01,
##                              colsample_bytree = 0.75,
##                              subsample = 0.5,
##                              min_child_weight = 0)
## 
## ## nonparametric kernel smoothing regression
## npreg <- list(
##   label = "Kernel regression",
##   type = "Regression",
##   library = "np",
##   parameters = data.frame(parameter =
##                             c("subsample", "regtype",
##                               "ckertype", "ckerorder"),
##                           class = c("integer", "character",
##                                     "character", "integer"),
##                           label = c("#subsample", "regtype",
##                                     "ckertype", "ckerorder")),
##   grid = function(x, y, len = NULL, search = "grid") {
##     if (!identical(search, "grid")) {
##       stop("No random search implemented.\n")
##     } else {
##       out <- expand.grid(subsample = c(50, 100),
##                          regtype = c("lc", "ll"),
##                          ckertype =
##                            c("gaussian",
##                              "epanechnikov",
##                              "uniform"),
##                          ckerorder = seq(2, 8, 2))
##     }
##     return(out)
##   },
##   fit = function(x, y, wts, param, lev, last, classProbs, ...) {
##     ny <- length(y)
##     if (ny > param$subsample) {
##       ## otherwise far too slow for what we intend to do here...
##       keep <- sample.int(ny, param$subsample)
##       x <- x[keep, ]
##       y <- y[keep]
##     }
##     bw <- np::npregbw(xdat = as.data.frame(x), ydat = y,
##                       regtype = param$regtype,
##                       ckertype = param$ckertype,
##                       ckerorder = param$ckerorder,
##                       remin = FALSE, ftol = 0.01, tol = 0.01,
##                       ...)
##     np::npreg(bw)
##   },
##   predict = function (modelFit, newdata, preProc = NULL, submodels = NULL) {
##     if (!is.data.frame(newdata)) {
##       newdata <- as.data.frame(newdata)
##     }
##     np:::predict.npregression(modelFit, se.fit = FALSE, newdata)
##   },
##   sort = function(x) {
##     x[order(x$regtype, x$ckerorder), ]
##   },
##   loop = NULL, prob = NULL, levels = NULL
## )
## 
## npreg_algo <- list(
##   algo = function(dat, ...) {
##     caret::train(working_model_Q_one$formula,
##                  data = dat,
##                  method = npreg, # no quotes!
##                  verbose = FALSE,
##                  ...)
##   },
##   type_of_preds = "response"
## )
## attr(npreg_algo, "ML") <- TRUE
## npreg_grid <- data.frame(subsample = 100,
##                          regtype = "lc",
##                          ckertype = "gaussian",
##                          ckerorder = 4,
##                          stringsAsFactors = FALSE)

