#' Trims the fat from a 'glm' object
#'
#' Imported from the 'glm' 'model' of the 'caret' package, trims the fat from a 'glm' object.
#' 
#' @param x A \code{glm} fit.
#'
#' @return Object 'fit' stripped of components that are not required to make predictions.
#'
#' @examples
#'
#' ## Dobson (1990) Page 93: Randomized Controlled Trial (see 'example(glm)'):
#' counts <- c(18,17,15,20,10,20,25,13,12)
#' outcome <- gl(3,1,9)
#' treatment <- gl(3,3)
#' dat <- data.frame(treatment, outcome, counts)
#' fit <- glm(counts ~ outcome + treatment, family = poisson())
#' fit_light <- trim_glm_fit(fit)
#' if (requireNamespace("pryr", quietly = TRUE)) {
#'   pryr::object_size(fit)
#'   pryr::object_size(fit_light)
#' } else {
#'   object.size(fit)
#'   object.size(fit_light)
#'}
#' stats::predict(fit_light, newdata = dat)
#'
#' @export
trim_glm_fit <- caret::getModelInfo("glm")$glm$trim

#' Estimates the Gbar feature of an object of class LAW
#'
#' Given a data set  consisting of realizations of (W,A,Y) in  [0,1] x {0,1} x
#' [0,1]  drawn   from  a  common  law,   \code{estimate_Gbar}  estimates  the
#' conditional probability that  A=1 given W, the so called  'Gbar' feature of
#' the law.
#'
#' @param  dat The learning data  set. Must have the  same form as a  data set
#'   produced    by    an    object   of    \code{class}    \code{LAW}    (see
#'   '?tlrider').
#'
#' @param  algorithm The  algorithm.  See the  built-in algorithms  by running
#'   'data(algorithms)'.
#'
#' @param ... Additional parameters passed to the algorithm.
#'
#' @seealso \code{\link{estimate_QW}} to estimate the marginal distribution of
#'   W, \code{\link{estimate_Qbar}} to estimate the conditional expectation of
#'   Y given (A,W).
#' 
#' @references Benkeser & Chambaz, "A Ride in Targeted Learning Territory" (2018).
#' 
#' @return The  result of  the fit  output by  the algorithm  trained on  the
#'   learning data set.
#'
#' @examples
#'
#' ## create an experiment and draw a data set from it
#' example(tlrider, echo = FALSE)
#' obs <- sample_from(experiment, n = 250)
#'
#' ## load the built-in algorithms
#' data(algorithms)
#' working_model_G_one
#'
#' ## estimate 'Gbar'
#' Gbar_hat <- estimate_Gbar(obs, working_model_G_one)
#' 
#' @export 
estimate_Gbar <- function(dat, algorithm, ...) {
  if (!is.data.frame(dat)) {
    dat <- as.data.frame(dat)
  }
  if (!attr(algorithm, "ML")) {
    fit <- algorithm[[1]](formula = algorithm[[2]], data = dat)
  } else {
    fit <- algorithm[[1]](dat, ...)
  }
  attr(fit, "type_of_preds") <- algorithm$type_of_preds
  return(fit)
}

#' Estimates the Qbar feature of an object of class LAW
#'
#' Given a data set  consisting of realizations of (W,A,Y) in  [0,1] x {0,1} x
#' [0,1]  drawn   from  a  common  law,   \code{estimate_Qbar}  estimates  the
#' conditional expectation of  Y given (A,W), the so called  'Qbar' feature of
#' the law.
#'
#' @param  dat The learning data  set. Must have the  same form as a  data set
#'   produced    by    an    object   of    \code{class}    \code{LAW}    (see
#'   '?tlrider').
#'
#' @param  algorithm The  algorithm.  See the  built-in algorithms  by running
#'   'data(algorithms)'.
#'
#' @param ... Additional parameters passed to the algorithm.
#'
#' @seealso \code{\link{estimate_QW}} to estimate the marginal distribution of
#'   W,  \code{\link{estimate_Gbar}} to  estimate the  conditional probability
#'   that A=1 given W.
#'
#' @references Benkeser & Chambaz, "A Ride in Targeted Learning Territory" (2018).
#' 
#' @return The  result of  the fit  output by  the algorithm  trained on  the
#'   learning data set.
#'
#' @examples
#'
#' ## create an experiment and draw a data set from it
#' example(tlrider, echo = FALSE)
#' obs <- sample_from(experiment, n = 250)
#'
#' ## load the built-in algorithms
#' data(algorithms)
#' working_model_Q_one
#'
#' ## estimate 'Qbar'
#' Qbar_hat <- estimate_Qbar(obs, working_model_Q_one)
#' 
#' @export 
estimate_Qbar <- function(dat, algorithm, ...) {
  if (!is.data.frame(dat)) {
    dat <- as.data.frame(dat)
  }
  if (!attr(algorithm, "ML")) {
    if (!attr(algorithm, "stratify")) {
      fit_both <- algorithm[[1]](formula = algorithm[[2]], data = dat)
      fit_one <- list()
      fit_zero <- list()
    } else {
      idx_one <- (dat$A == 1)
      if (sum(idx_one) %in% c(0, nrow(dat))) {
        stop("Impossible to stratify.\n")
      }
      fit_both <- list()
      fit_one <- algorithm[[1]](formula = algorithm[[2]], data = dat[idx_one, ])
      fit_zero <- algorithm[[1]](formula = algorithm[[2]], data = dat[!idx_one, ])
    }
  } else {
    if (!attr(algorithm, "stratify")) {
      fit_both <- algorithm[[1]](dat, ...)
      fit_one <- list()
      fit_zero <- list()
    } else {
      idx_one <- (dat$A == 1)
      if (sum(idx_one) %in% c(0, nrow(dat))) {
        stop("Impossible to stratify.\n")
      }
      fit_both <- list()
      fit_one <- algorithm[[1]](dat[idx_one, ], ...)
      fit_zero <- algorithm[[1]](dat[!idx_one, ], ...)
    }
  }
  fit <- dplyr::tibble(a = c("both", "one", "zero"),
                       fit = list(fit_both, fit_one, fit_zero))
  attr(fit, "type_of_preds") <- algorithm$type_of_preds
  attr(fit, "stratify") <- attr(algorithm, "stratify")
  return(fit)
}

#' Estimates the marginal law of W non-parametrically
#'
#' Given a data set  consisting of realizations of (W,A,Y) in  [0,1] x {0,1} x
#' [0,1]  drawn   from  a   common  law,  \code{estimate_QW}   estimates  non-
#' parametrically the marginal law of W, the so called 'QW' feature.
#' 
#' @param dat The learning data  set. Must have the  same form as a  data set
#'   produced    by    an    object   of    \code{class}    \code{LAW}    (see
#'   '?tlrider').
#'
#' @seealso   \code{\link{estimate_Gbar}}   to  estimate   the   conditional
#'   probability that A=1 given W, \code{\link{estimate_Qbar}} to estimate the
#'   conditional expectation of Y given (A,W).
#'
#' @references Benkeser & Chambaz, "A Ride in Targeted Learning Territory" (2018).
#' 
#' @return A  \code{tibble}  with  columns named  'value'  and 'weight'  that
#'   describes the empirical law of 'W' in data set 'dat'.
#'
#' @examples
#'
#' ## create an experiment and draw a data set from it
#' example(tlrider, echo = FALSE)
#' obs <- sample_from(experiment, n = 250)
#'
#' ## estimate 'QW'
#' QW_hat <- estimate_QW(obs)
#' 
#' 
#' @export 
estimate_QW <- function(dat) {
  dat %>% tibble::as.tibble() %>%
    dplyr::select(value = "W") %>%
    dplyr::mutate(weight = 1/dplyr::n())
}

#' Computes the conditional likelihood of A_i given W_i
#'
#' Given  realizations of  (W,A) in  [0,1]  x {0,1}  and an  estimator of  the
#' conditional    probability   that    A=1    given   W    (as   output    by
#' \code{estimate_Gbar}), \code{compute_lGbar_hatAW}  computes the conditional
#' probability that A=A_i given W=W_i.
#'
#' @param A A \code{vector} of 0 and 1.
#'
#' @param W A \code{vector} of real numbers between 0 and 1.
#'
#' @param Gbar_hat The output  of \code{estimate_Gbar}, derived by training an
#'   algorithm on a learning data set (see '?estimate_Gbar').
#'
#' @param threshold A \code{numeric} taking values between 0 and 1/2 (defaults
#'   to 0.05).
#'
#' @return  A \code{vector} containing  'lGAW' defined as the  maximum between
#'   'threshold' and\deqn{A  * Gbar_hat(W)  + (1  - A)  * (1  - Gbar_hat(W)),}
#'   where \eqn{Gbar_hat(W)} is  the conditional probability that  A=1 given W
#'   predicted by the fit 'Gbar_hat'.
#'
#' @family estimating functions
#'
#' @references Benkeser & Chambaz, "A Ride in Targeted Learning Territory" (2018).
#' 
#' @export
compute_lGbar_hatAW <- function(A, W, Gbar_hat, threshold = 0.05) {
  threshold <- R.utils::Arguments$getNumeric(threshold, c(0, 1/2))
  dat <- data.frame(A = A, W = W)
  GW <- stats::predict(Gbar_hat, newdata = dat, type = attr(Gbar_hat, "type_of_preds"))
  lGAW <- A * GW + (1 - A) * (1 - GW)
  pred <- pmin(1 - threshold, pmax(lGAW, threshold))
  return(pred)
}

#' Computes the conditional probability of A = 1 given W=W_i
#'
#' Given  realizations of  W  in [0,1]  and an  estimator  of the  conditional
#' probability  that   A=1  given  W  (as   output  by  \code{estimate_Gbar}),
#' \code{compute_Gbar_hatW}  computes  the  conditional probability  that  A=1
#' given W=W_i.
#'
#' @param W A \code{vector} of real numbers between 0 and 1.
#'
#' @param Gbar_hat The output  of \code{estimate_Gbar}, derived by training an
#'   algorithm on a learning data set (see '?estimate_Gbar').
#'
#' @param threshold A \code{numeric} taking values between 0 and 1/2 (defaults
#'   to 0.05).
#'
#'@return  A \code{vector}  containing  'GW'  defined as  \eqn{Gbar_hat(W)}
#'   bounded away from 0 and 1  by 'threshold', where \eqn{Gbar_hat(W)} is the
#'   conditional probability that A=1 given W predicted by the fit 'Gbar_hat'.
#'
#' @family estimating functions
#'
#' @references Benkeser & Chambaz, "A Ride in Targeted Learning Territory" (2018).
#' 
#' @export
compute_Gbar_hatW <- function(W, Gbar_hat, threshold = 0.05) {
  threshold <- R.utils::Arguments$getNumeric(threshold, c(0, 1/2))
  dat <- data.frame(W = W)
  GW <- stats::predict(Gbar_hat, newdata = dat, type = attr(Gbar_hat, "type_of_preds"))
  pred <- pmin(1 - threshold, pmax(GW, threshold))
  return(pred)
}


#' Computes the conditional expectation of Y given (A,W)=(A_i,W_i)
#'
#' Given  realizations of  (W,A) in  [0,1]  x {0,1}  and an  estimator of  the
#' conditional    expectation   of    Y    given   (A,W)    (as   output    by
#' \code{estimate_Qbar}),  \code{compute_Qbar_hatAW} computes  the conditional
#' expectation of Y  given (A,W)=(A_i,W_i).
#'
#' @param A A \code{vector} of 0 and 1.
#'
#' @param W A \code{vector} of real numbers between 0 and 1.
#'
#' @param Qbar_hat The output  of \code{estimate_Qbar}, derived by training an
#'   algorithm on a learning data set (see '?estimate_Qbar').
#'
#' @param  blip A \code{logical}  (defaults to 'FALSE') indicating  whether or
#'   not to output the so called blip function (see below).
#'
#' @return  A \code{vector}  containing either \eqn{Qbar_hat(A,W)}  (ig 'blip'
#'   argument  set to  'FALSE') or  \eqn{Qbar_hat(1,W) -  Qbar_hat(0,W)} where
#'   \eqn{Qbar_hat(A,W)}  is  the conditional  expectation  of  Y given  (A,W)
#'   predicted by the fit 'Qbar_hat'.
#'
#' @family estimating functions
#'
#' @references Benkeser & Chambaz, "A Ride in Targeted Learning Territory" (2018).
#' 
#' @export
compute_Qbar_hatAW <- function(A, W, Qbar_hat, blip = FALSE) {
  blip <- R.utils::Arguments$getLogical(blip)
  stratify <- R.utils::Arguments$getLogical(attr(Qbar_hat, "stratify"))
  if (!blip) {
    dat <- data.frame(Y = NA, A = A, W = W)
    if (!stratify) {
      fit <- Qbar_hat %>% dplyr::filter(.data$a == "both") %>%
        dplyr::pull("fit") %>% dplyr::first()
      pred <- stats::predict(fit,
                             newdata = dat,
                             type = attr(Qbar_hat, "type_of_preds"))
    } else {
      fit_one <- Qbar_hat %>% dplyr::filter(.data$a == "one") %>%
        dplyr::pull("fit") %>% dplyr::first()
      fit_zero <- Qbar_hat %>% dplyr::filter(.data$a == "zero") %>%
        dplyr::pull("fit") %>% dplyr::first()
      pred <- vector("numeric", nrow(dat))
      idx_one <- (dat$A == 1)
      if (sum(idx_one) > 0) {
        pred[idx_one] <- stats::predict(fit_one,
                                        newdata = dat[idx_one, ],
                                        type = attr(Qbar_hat, "type_of_preds"))
      }
      if (sum(!idx_one) > 0) {
        pred[!idx_one] <- stats::predict(fit_zero,
                                         newdata = dat[!idx_one, ],
                                         type = attr(Qbar_hat, "type_of_preds"))
      }
    }
  } else {
    if (!stratify) {
      fit <- Qbar_hat %>% dplyr::filter(.data$a == "both") %>%
        dplyr::pull("fit") %>% dplyr::first()
      pred <- stats::predict(fit, newdata = data.frame(A = 1, W = W),
                             type = attr(Qbar_hat, "type_of_preds")) -
        stats::predict(fit, newdata = data.frame(A = 0, W = W),
                       type = attr(Qbar_hat, "type_of_preds"))
    } else {
      fit_one <- Qbar_hat %>% dplyr::filter(.data$a == "one") %>%
        dplyr::pull("fit") %>% dplyr::first()
      fit_zero <- Qbar_hat %>% dplyr::filter(.data$a == "zero") %>%
        dplyr::pull("fit") %>% dplyr::first()
      pred <- stats::predict(fit_one, newdata = data.frame(A = 1, W = W),
                             type = attr(Qbar_hat, "type_of_preds")) -
        stats::predict(fit_zero, newdata = data.frame(A = 0, W = W),
                       type = attr(Qbar_hat, "type_of_preds"))
    }
  }
  return(pred)  
}

#' A convenient wrapper to make predictions based on a fit
#'
#' A convenient wrapper to make predictions  based on a fit produced by either
#' \code{estimate_Gbar} or \code{estimate_Qbar}.
#'
#' @param fit A fit as output by \code{estimate_Gbar} or \code{estimate_Qbar}.
#'
#' @param unenclose A \code{logical}  indicating whether or not  the function
#'   should be  \code{pryr::unenclose}d ("yes"  in general,  "no" in  calls to
#'   \code{mutate} for instance). Defaults to \code{TRUE}.
#' 
#' @return A function to make predictions.
#'
#' @family estimating functions
#'
#' @export
wrapper <- function(fit, unenclose = TRUE) {
  unenclose <- R.utils::Arguments$getLogical(unenclose)
  type_of_preds <- R.utils::Arguments$getCharacter(attr(fit, "type_of_preds"))
  if (!is.null(attr(fit, "stratify"))) {
    ## argument 'fit' is an output of 'estimate_Qbar'
    stratify <- R.utils::Arguments$getLogical(attr(fit, "stratify"))
    if (!stratify) {
      fit <- fit %>% dplyr::filter(.data$a == "both") %>%
        dplyr::pull("fit") %>% dplyr::first()
      out <- function(obs) {
        obs <- as.data.frame(obs)
        stats::predict(fit,
                       newdata = obs,
                       type = type_of_preds)
      }
    } else {
      fit_one <- fit %>% dplyr::filter(.data$a == "one") %>%
        dplyr::pull("fit") %>% dplyr::first()
      fit_zero <- fit %>% dplyr::filter(.data$a == "zero") %>%
        dplyr::pull("fit") %>% dplyr::first()
      out <- function(obs) {
        obs <- as.data.frame(obs)
        pred <- vector("numeric", nrow(obs))
        idx_one <- (obs$A == 1)
        if (sum(idx_one) > 0) {
          pred[idx_one] <- stats::predict(fit_one,
                                          newdata = obs[idx_one, ],
                                          type = type_of_preds)
        }
        if (sum(!idx_one) > 0) {
          pred[!idx_one] <- stats::predict(fit_zero,
                                           newdata = obs[!idx_one, ],
                                           type = type_of_preds)
        }
      }
    }
  } else {
    ## argument 'fit' is an output of 'estimate_Gbar'
    out <- function(obs) {
      obs <- as.data.frame(obs)
      stats::predict(fit,
                     newdata = obs,
                     type = type_of_preds)
    }
  }
  if (unenclose) {
    out <- pryr::unenclose(out)
  }
  return(out)
}

#' Builds the IPTW estimator
#'
#' Given a data set  consisting of realizations of (W,A,Y) in  [0,1] x {0,1} x
#' [0,1] drawn  from a  common law  and the  conditional probability  that A=1
#' given W, the so called 'Gbar' feature  of the law, either a priori known or
#' estimated  beforehand, \code{compute_iptw}  builds  the  IPTW estimator  of
#' \eqn{Psi} at the law that generated the data set.
#'
#' @param  dat The learning data  set. Must have the  same form as a  data set
#'   produced    by    an    object   of    \code{class}    \code{LAW}    (see
#'   '?tlrider').
#'
#' @param  Gbar  The  conditional  probability  that  'A=1'   given  'W',  a
#'   \code{function}.
#'
#' @param  threshold A small  positive number (default  value 0.05) used  to bound
#'   'Gbar' away from zero.
#' 
#' @seealso \code{\link{estimate_Gbar}}   to  estimate   the   conditional
#'   probability that A=1 given  W, \code{\link{compute_gcomp}} to compute the
#'   G-computation estimator.
#'
#' @references Benkeser & Chambaz, "A Ride in Targeted Learning Territory" (2018).
#' 
#' @return  A  \code{tibble} containing  the  value  of the  IPTW  estimator
#'   ('psi_n'  column) and  that of  the estimator  of its  standard deviation
#'   ('sig_n' column).
#'
#' @details  Caution: when the 'Gbar'  feature is estimated, the  estimator of
#'   the  standard deviation  of  the IPTW  estimator  is conservative,  i.e.,
#'   over-estimates its target.
#'
#' @examples
#'
#' ## create an experiment and draw a data set from it
#' example(tlrider, echo = FALSE)
#' obs <- sample_from(experiment, n = 250)
#'
#' ## suppose that 'Gbar' is known
#' Gbar <- get_feature(experiment, "Gbar")
#'
#' ## compute the IPTW estimator
#' (compute_iptw(obs, Gbar))
#' 
#' @export 
compute_iptw <- function(dat, Gbar, threshold = 0.05) {
  threshold <- R.utils::Arguments$getNumeric(threshold, c(0, 1/2))
  W <- dat[, "W", drop = FALSE]
  A <- dat[, "A", drop = FALSE]
  Y <- dat[, "Y"]
  lGAW <- A * Gbar(W) + (1 - A) * (1 - Gbar(W))
  lGAW <- pmax(threshold, lGAW)
  psi_n <- mean(Y * (2 * A - 1) / lGAW)
  sig_n <- stats::sd(Y * (2 * A - 1) / lGAW) / sqrt(nrow(dat))
  tibble::tibble(psi_n = psi_n, sig_n = sig_n)
}
  
#' Builds the G-computation estimator
#'
#' Given a marginal law  of W and a conditional expectation  of Y given (A,W),
#' the so called 'GW' and 'Qbar' features  of the law of an experiment, either
#' a  priori known  or estimated  beforehand, \code{compute_gcomp}  builds the
#' G-computation estimator  of \eqn{Psi}  at that law.
#'
#' @param QW  The marginal  law of  'W', a  \code{tibble} with  columns named
#'   'value' and 'weight' (a discrete law) as output by \code{estimate_QW}.
#' 
#' @param  Qbar   The  conditional  expectation of 'Y' given '(A,W)', a \code{function}.  
#'
#' @param nobs The sample size of the data set used to estimate the above 'QW'
#'   and  'Qbar', an  \code{integer}.  Used  only  for the  estimation of  the
#'   standard deviation of the G-computation  estimator, which is not reliable.
#'   
#' @seealso \code{\link{estimate_QW}}  to  estimate the  marginal  law of  W,
#'   \code{\link{estimate_Qbar}} to estimate  the conditional expectation of Y
#'   given  (A,W), \code{\link{compute_iptw}}  to compute the  IPTW estimator,
#'   \code{\link{wrapper}}.
#'
#' @references Benkeser & Chambaz, "A Ride in Targeted Learning Territory" (2018).
#' 
#' @return  A  \code{tibble} containing  the  value  of the  G-computation  estimator
#'   ('psi_n'  column) and  that of  the estimator  of its  standard deviation
#'   ('sig_n' column).
#'
#' @details Caution:  the estimator  of the  standard deviation  of the  G-computation
#'   estimator can be trusted only in very specific circumstances.
#' 
#' @examples
#'
#' ## create an experiment and draw a data set from it
#' example(tlrider, echo = FALSE)
#' obs <- sample_from(experiment, n = 250)
#'
#' ## estimate 'QW' and 'Qbar'
#' QW_hat <- estimate_QW(obs)
#' Qbar_hat <- estimate_Qbar(obs, working_model_Q_one)
#'
#' ## wrap 'Qbar_hat' (a fit) into a function
#' Qbar_hat_fun <- wrapper(Qbar_hat, FALSE)
#' 
#' ## compute the G-computation estimator
#' (compute_gcomp(QW_hat, Qbar_hat_fun, nrow(obs)))
#' 
#' @export 
compute_gcomp <- function(QW, Qbar, nobs) {
  nobs <- R.utils::Arguments$getInteger(nobs, c(1, Inf))
  if (FALSE) {## does not work, unfortunately
    xprmnt <- LAW()
    alter(xprmnt, "QW" = QW)
    alter(xprmnt, "Qbar" = Qbar)
    psi_n <- evaluate_psi(xprmnt)
  }
  if (!identical(names(QW), c("value", "weight"))) {
    stop(stringr::str_c("Argument 'QW' is not a valid discrete law.\n"))
  }
  W <- dplyr::pull(QW, "value")
  out <- Qbar(cbind(A = 1, W = W)) - Qbar(cbind(A = 0, W = W))
  psi_n <- stats::weighted.mean(out, dplyr::pull(QW, "weight"))
  sig_n <- stats::weighted.mean((out - psi_n)^2, dplyr::pull(QW, "weight"))
  sig_n <- sqrt(sig_n)/sqrt(nobs)
  tibble::tibble(psi_n = psi_n, sig_n = sig_n)
}

#' Applies a one-step correction to an initial estimator
#'
#' Given an  (initial) substitution  estimator of \eqn{\Psi}  at the  law that
#' generated    the    data    set    used    to    build    the    estimator,
#' \code{apply_one_step_correction}  updates  it  in  one  single  step.   The
#' correction exploits\itemize{\item the so called  'Gbar' feature of the law,
#' either a  priori known or  estimated \item the  estimator of the  so called
#' 'Qbar' feature  of the law  that was built and  used to derive  the initial
#' estimator \item the initial estimator \item  the data set used to infer the
#' above features and parameter.}
#'
#' @param  dat The learning data  set. Must have the  same form as a  data set
#'   produced by an object of \code{class} \code{LAW} (see '?tlrider').
#'
#' @param  Gbar  The actual  'Gbar'  feature  of  the  law, if  it  is  known
#'   beforehand, or an estimator  thereof, the output of \code{estimate_Gbar},
#'   derived  by   training  an  algorithm   on  a  learning  data   set  (see
#'   '?estimate_Gbar').
#'
#' @param Qbar  The output  of \code{estimate_Qbar},  derived by  training an
#'   algorithm on a learning data set (see '?estimate_Qbar').
#'
#' @param psi A \code{numeric}, the estimator to be corrected in one step.
#'
#' @seealso   \code{\link{estimate_Gbar}}   to  estimate   the   conditional
#'   probability that A=1 given W, \code{\link{estimate_Qbar}} to estimate the
#'   conditional expectation of Y  given (A,W), \code{\link{compute_gcomp}} to
#'   compute the G-computation estimator, \code{\link{wrapper}}.
#'
#' @references Benkeser & Chambaz, "A Ride in Targeted Learning Territory" (2018).
#' 
#' @return A \code{tibble} containing  the value of updated estimator ('psi_n'
#'   column)  and that  of the  estimator of  its standard  deviation ('sig_n'
#'   column).
#'
#' @details Caution:  the estimator of the standard deviation  of the updated
#'   estimator can be trusted only in very specific circumstances.
#' 
#' @examples
#'
#' ## create an experiment and draw a data set from it
#' example(tlrider, echo = FALSE)
#' obs <- sample_from(experiment, n = 250)
#'
#' ## estimate 'QW', 'Gbar' and 'Qbar'
#' QW_hat <- estimate_QW(obs)
#' Gbar_hat <- estimate_Gbar(obs, working_model_G_one)
#' Qbar_hat <- estimate_Qbar(obs, working_model_Q_one)
#'
#' ## wrap 'Gbar_hat' and 'Qbar_hat' (two fits) into two functions
#' Gbar_hat_fun <- wrapper(Gbar_hat, FALSE)
#' Qbar_hat_fun <- wrapper(Qbar_hat, FALSE)
#' 
#' ## compute the G-computation estimator
#' psi_hat <- compute_gcomp(QW_hat, Qbar_hat_fun, nrow(obs))
#'
#' ## apply the one-step correction
#' (apply_one_step_correction(obs, Gbar_hat_fun, Qbar_hat_fun, psi_hat))
#' 
#' @export 
apply_one_step_correction <- function(dat, Gbar, Qbar, psi) {
  eic <- function(obs) {
    if (length(intersect(c("W", "A", "Y"), names(obs))) == 3) {
      stop(stringr::str_c("Argument 'obs' of ",
                          deparse(substitute(this)),
                          " should contain columns named 'W', 'A' and 'Y'.\n"))
    } 
    QAW <- Qbar(obs[, c("A", "W")])
    QoneW <- Qbar(cbind(A = 1, W = obs[, "W"]))
    QzeroW <- Qbar(cbind(A = 0, W = obs[, "W"]))
    GW <- Gbar(obs[, "W", drop = FALSE])
    lGAW <- obs[, "A"] * GW + (1 - obs[, "A"]) * (1 - GW)
    out <- (QoneW - QzeroW - psi$psi_n) + (2 * obs[, "A"] - 1) / lGAW * (obs[, "Y"] - QAW)
    out <- as.vector(out)
    return(out)
  }
  eic_dat <- eic(dat)
  psi_n <- psi$psi_n + mean(eic_dat)
  sig_n <- sd(eic_dat)/sqrt(nrow(dat))
  tibble::tibble(psi_n = psi_n, sig_n = sig_n)
}


