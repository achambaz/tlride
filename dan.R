## ----install-package, eval = FALSE---------------------------------------
## devtools::install_github("achambaz/gttmle/guided.tour.tmle")

## ----visible-setup, results = FALSE--------------------------------------
set.seed(54321) ## because reproducibility matters...
library(tidyverse)
library(caret)
library(ggdag)
library(guided.tour.tmle)

## ----redo----------------------------------------------------------------
redo_fixed <- c(TRUE, FALSE)[1]
redo_varying <- c(TRUE, FALSE)[1]
## if  'redo_$'  then  recompute  'learned_features_$_sample_size',  otherwise
## upload it if it is not already in the environment.
if (!redo_fixed) {
  if (!exists("learned_features_fixed_sample_size")) {
    learned_features_fixed_sample_size <-
      load("../tmle-applique/data/learned_features_fixed_sample_size_new.xdr")
  }
}
if (!redo_varying) {
  if (!exists("learned_features_varying_sample_size")) {
    learned_features_varying_sample_size <-
      load("../tmle-applique/data/learned_features_varying_sample_size_new.xdr")
  }
} 

## ----example-one, results = FALSE----------------------------------------
example(guided.tour.tmle)

## ----example-two---------------------------------------------------------
ls()

## ----view-experiment-----------------------------------------------------
experiment

## ----draw-five-obs-------------------------------------------------------
(five_obs <- sample_from(experiment, n = 5))

## ----exercise:visualize, eval = TRUE-------------------------------------
some_relevant_features <- reveal(experiment)
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
(psi_zero <- evaluate_psi(experiment))

## ----DAG, out.width = '70%', fig.align = 'center', fig.width = 8, fig.height = 6, fig.cap = '(ref:DAG)'----
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
ideal_obs <- sample_from(experiment, B, ideal = TRUE)
(psi_approx <- mean(ideal_obs[, "Yone"] - ideal_obs[, "Yzero"]))

## ----approx-psi-zero-b---------------------------------------------------
sd_approx <- sd(ideal_obs[, "Yone"] - ideal_obs[, "Yzero"])
alpha <- 0.05
(psi_approx_CI <- psi_approx + c(-1, 1) * qnorm(1 - alpha / 2) * sd_approx / sqrt(B))

## ----another-simulation--------------------------------------------------
run_another_experiment <- function(n, h = 0) {
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
  attr(obs, "qY") <- function(AW, Y, Qbar, shape1){
    A <- AW[,1]; W <- AW[,2]
    Qbar.AW <- do.call(Qbar, list(AW))
    dbeta(Y, shape1 = shape1, shape2 = shape1 * (1 - Qbar.AW) / Qbar.AW)
  }
  ##
  return(obs)
}

## ----approx-psi-one------------------------------------------------------
(five_obs_another_experiment <- sample_from(another_experiment, 5, h = 0))
(psi_Pi_zero <- evaluate_psi(another_experiment, h = 0))

## ----psi-approx-psi-one, fig.cap = '(ref:psi-approx-psi-one)'------------
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

## ----set-Q-bar, eval = FALSE---------------------------------------------
## Qbar = function(AW, hh = h){
##   A <- AW[,1]
##   W <- AW[,2]
##   expit( logit( A * W + (1 - A) * W^2 ) +
##          hh * (2*A - 1) / ifelse(A == 1, sin((1 + W) * pi / 6),
##                                  1 - sin((1 + W) * pi / 6)) *
##          (Y - A * W + (1 - A) * W^2))
## }

## ----eic-----------------------------------------------------------------
## 
## move around
## 
eic_experiment <- evaluate_eic(experiment)
(eic_experiment(five_obs))

eic_another_experiment <- evaluate_eic(another_experiment, h = 0)
(eic_another_experiment(five_obs_another_experiment))

## ----cramer-rao----------------------------------------------------------
##
## move around
##
obs <- sample_from(experiment, B)
(cramer_rao_hat <- var(eic_experiment(obs)))

## ----cramer-rao-another-experiment---------------------------------------
obs_another_experiment <- sample_from(another_experiment, B, h = 0)
(cramer_rao_Pi_zero_hat <- var(eic_another_experiment(obs_another_experiment)))
(ratio <- sqrt(cramer_rao_Pi_zero_hat/cramer_rao_hat))

## ----recover-slope-------------------------------------------------------
vars <- eic_another_experiment(obs_another_experiment) *
  sigma0(obs_another_experiment)
sd_hat <- sd(vars)
(slope_hat <- mean(vars))
(slope_CI <- slope_hat + c(-1, 1) * qnorm(1 - alpha / 2) * sd_hat / sqrt(B))

## ----evaluating-remainder------------------------------------------------
# Could we add a method for evaluating the remainder?
# e.g., using integrate and/or approximating with large sample

## ----draw-a-sample-------------------------------------------------------
## Debug -- couldn't find obs when I tried to compile
## obs <- run_experiment(B)

## ----intro-est-G---------------------------------------------------------
## estimate_G <- function(dat, algorithm, ...) {
##   if (!is.data.frame(dat)) {
##     dat <- as.data.frame(dat)
##   }
##   if (!attr(algorithm, "ML")) {
##     fit <- algorithm[[1]](formula = algorithm[[2]], data = dat)
##   } else {
##     fit <- algorithm[[1]](dat, ...)
##   }
##   fit$type_of_preds <- algorithm$type_of_preds
##   return(fit)
## }

## ----unknown-Gbar-two----------------------------------------------------
trim_glm_fit <- caret::getModelInfo("glm")$glm$trim
working_model_G_one <- list(
  model = function(...) { 
    trim_glm_fit(glm(family = binomial(), ...))
  },
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

## ----show-estimateG-in-action--------------------------------------------


## ----intro-estimateQ-method----------------------------------------------
# introduce code for estimation of $\Qbar_0$ 
# and show how it works 

## ----intro-to-estimateQW-------------------------------------------------
# not sure how this is being handled in the code 
# and whether we need to introduce a function for this here?

## ----show-g-comp-computation---------------------------------------------


## ----show-another-gcomp--------------------------------------------------


## ----simulation-with-parametric-and-knn----------------------------------
# maybe have three estimators? one with correct parametric model, one with misspecified parametric model and one with knn?

## ----show-rootn-times-PnDstar-blow up------------------------------------


## ----show-one-step-in-simulation-----------------------------------------


## ----show-weak-convergence-----------------------------------------------


## ----show-confidence-intervals-------------------------------------------


## ----plot-for-several-epsilon--------------------------------------------
# can include the graph of Qbar_n,eps(a,W) ~ W for eps = -1, 0, 1 \times a = 0,1?
# would like to drive home the point that each epsilon is simply a different regresion
# fit.

# could also show for different Gbar_n showing that the submodel depends on Gbar_n, 
# but I actually think that's far less important. 

## ----compute-negloglik-and-plot------------------------------------------
# basically just do a grid search over epsilon for minimizer of negative log-likelihood loss

## ----known-Gbar-one-a----------------------------------------------------
Gbar <- get_feature(experiment, "Gbar")
iter <- 1e3

## ----known-Gbar-one-b, fig.cap = '(ref:known-Gbar-one-b)'----------------
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
## estimate_Gbar <- function(dat, algorithm, ...) {
##   if (!is.data.frame(dat)) {
##     dat <- as.data.frame(dat)
##   }
##   if (!attr(algorithm, "ML")) {
##     fit <- algorithm[[1]](formula = algorithm[[2]], data = dat)
##   } else {
##     fit <- algorithm[[1]](dat, ...)
##   }
##   fit$type_of_preds <- algorithm$type_of_preds
##   return(fit)
## }

## compute_lGbar_hatAW <- function(A, W, Ghat, threshold = 0.05) {
##   dat <- data.frame(A = A, W = W)
##   Ghat_W <- predict(Ghat, newdata = dat, type = Ghat$type_of_preds)
##   lGAW <- A * Ghat_W + (1 - A) * (1 - Ghat_W)
##   pred <- pmin(1 - threshold, pmax(lGAW, threshold))
##   return(pred)
## }

## ----fake----------------------------------------------------------------
## empty...

## ----unknown-Gbar-two-bis------------------------------------------------
if (redo_fixed) {
  learned_features_fixed_sample_size <-
    obs %>% as_tibble() %>%
    mutate(id = (seq_len(n()) - 1) %% iter) %>%
    nest(-id, .key = "obs") %>%
    mutate(Gbar_hat = map(obs, ~ estimate_Gbar(., algorithm = working_model_G_one))) %>%
    mutate(lGAW = map2(Gbar_hat, obs, ~ compute_lGbar_hatAW(.y$A, .y$W, .x)))
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

## DEBUG : This was breaking when I compiled.
(bias_abc <- psi_hat_abc %>% group_by(type) %>% summarise(bias = mean(clt)))

## ----unknown-Gbar-three, fig.cap = '(ref:unknown-Gbar-three)'------------
fig +
  geom_density(aes(clt, fill = type, colour = type), psi_hat_abc, alpha = 0.1) +
  geom_vline(aes(xintercept = bias, colour = type),
             bias_abc, size = 1.5, alpha = 0.5) +
  xlim(-3, 4) + 
  labs(y = "",
       x = expression(paste(sqrt(n/v[n]^{list(a, b, c)})*
                            (psi[n]^{list(a, b, c)} - psi[0]))))

## ----unknown-Gbar-four, fig.cap = '(ref:unknown-Gbar-four)'--------------
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
## estimate_Q <- function(dat, algorithm, ...) {
##   if (!is.data.frame(dat)) {
##     dat <- as.data.frame(dat)
##   }
##   if (!attr(algorithm, "ML")) {
##     fit <- algorithm[[1]](formula = algorithm[[2]], data = dat)
##   } else {
##     fit <- algorithm[[1]](dat, ...)
##   }
##   fit$type_of_preds <- algorithm$type_of_preds
##   return(fit)
## }

## compute_QhatAW <- function(Y, A, W, Qhat, blip = FALSE) {
##   if (!blip) {
##     dat <- data.frame(Y = Y, A = A, W = W)
##     pred <- predict(Qhat, newdata = dat, type = Qhat$type_of_preds)
##   } else {
##     pred <- predict(Qhat, newdata = data.frame(A = 1, W = W),
##                     type = Qhat$type_of_preds) -
##       predict(Qhat, newdata = data.frame(A = 0, W = W),
##               type = Qhat$type_of_preds)
##   }
##   return(pred)  
## }

## working_model_Q_one <- list(
##   model = function(...) {trim_glm_fit(glm(family = binomial(), ...))},
##   formula = as.formula(
##     paste("Y ~ A * (",
##           paste("I(W^", seq(1/2, 3/2, by = 1/2), sep = "", collapse = ") + "),
##           "))")
##   ),
##   type_of_preds = "response"
## )
## attr(working_model_Q_one, "ML") <- FALSE
## working_model_Q_one$formula

## ## k-NN
## kknn_algo <- list(
##   algo = function(dat, ...) {
##     args <- list(...)
##     if ("Subsample" %in% names(args)) {
##       keep <- sample.int(nrow(dat), args$Subsample)
##       dat <- dat[keep, ]
##     }
##     fit <- caret::train(Y ~ I(10*A) + W, ## a tweak
##                         data = dat,
##                         method = "kknn",
##                         verbose = FALSE,
##                         ...)
##     fit$finalModel$fitted.values <- NULL
##     ## nms <- names(fit$finalModel$data)
##     ## for (ii in match(setdiff(nms, ".outcome"), nms)) {
##     ##   fit$finalModel$data[[ii]] <- NULL
##     ## }
##     fit$trainingData <- NULL    
##     return(fit)
##   },
##   type_of_preds = "raw"
## )
## attr(kknn_algo, "ML") <- TRUE
## kknn_grid <- expand.grid(kmax = 5, distance = 2, kernel = "gaussian")
## control <- trainControl(method = "cv", number = 2,
##                         predictionBounds = c(0, 1),
##                         trim = TRUE,
##                         allowParallel = TRUE)

## ----estimating-Qbar-one-bis, fig.cap = '(ref:estimating-Qbar-one-bis)'----
##
## not updated yet
##
if(redo_fixed) {
  learned_features_fixed_sample_size <-
    learned_features_fixed_sample_size %>% # head(n = 100) %>%
    mutate(Qbar_hat_d = map(obs, ~ estimate_Qbar(., algorithm = working_model_Q_one)),
           Qbar_hat_e = map(obs, ~ estimate_Qbar(., algorithm = kknn_algo,
                                          trControl = kknn_control,
                                          tuneGrid = kknn_grid))) %>%
    mutate(blip_QW_d = map2(Qbar_hat_d, obs,
                            ~ compute_Qbar_hatAW(.y$A, .y$W, .x, blip = TRUE)),
           blip_QW_e = map2(Qbar_hat_e, obs,
                            ~ compute_Qbar_hatAW(.y$A, .y$W, .x, blip = TRUE)))
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
##
## not updated yet
##

sample_size <- c(4e3, 9e3)
block_size <- sum(sample_size)


if (redo_varying) {
  learned_features_varying_sample_size <- obs %>% as.tibble %>% 
    head(n = (nrow(.) %/% block_size) * block_size) %>% 
    mutate(block = label(1:nrow(.), sample_size)) %>%
    nest(-block, .key = "obs")
} 

## ----estimating-Qbar-three, eval = TRUE----------------------------------
##
## not updated yet
## 
if(redo_varying) {
  learned_features_varying_sample_size <-
    learned_features_varying_sample_size %>% 
    mutate(Qbar_hat_d = map(obs, ~ estimate_Qbar(., algorithm = working_model_Q_one)),
           Qbar_hat_e = map(obs, ~ estimate_Qbar(., algorithm = kknn_algo,
                                          trControl = kknn_control,
                                          tuneGrid = kknn_grid))) %>%
    mutate(blip_QW_d = map2(Qbar_hat_d, obs,
                            ~ compute_Qbar_hatAW(.y$A, .y$W, .x, blip = TRUE)),
           blip_QW_e = map2(Qbar_hat_e, obs,
                            ~ compute_Qbar_hatAW(.y$A, .y$W, .x, blip = TRUE)))
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

## ----estimating-Qbar-four, fig.width = 5, fig.height = 5, fig.cap = '(ref:estimating-Qbar-four)'----
##
## not updated yet
##
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
##
## not updated yet
##
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

## ----one-step-two, fig.cap = '(ref:one-step-two)'------------------------
##
## not updated yet
##
psi_hat_de_one_step <- learned_features_fixed_sample_size %>%
  mutate(est_d = map(blip_QW_d, mean),
         est_e = map(blip_QW_e, mean)) %>%
  mutate(eic_obs_d = pmap(list(obs, Qbar_hat_d, Gbar_hat, est_d),
                          eic_hat),
         eic_obs_e = pmap(list(obs, Qbar_hat_e, Gbar_hat, est_e),
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
##
## not updated yet
##
bind_rows(bias_de, bias_de_one_step)

## ----enhance-------------------------------------------------------------
##
## not updated yet
##
psi_hat_de %>%
  full_join(psi_hat_de_one_step) %>% group_by(type) %>%
  summarize(sd = mean(std * ifelse(str_detect(type, "one_step"), 1, NA),
                      se = sd(est) * sqrt(n()),
                      mse = mean((est - psi_approx)^2) * n()))

