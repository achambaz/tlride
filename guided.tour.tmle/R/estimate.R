#' Trims the fat from a 'glm' object
#'
#' Imported from the 'glm' 'model' of the 'caret' package, trims the fat from a 'glm' object.
#' 
#' @param x A \code{glm} fit.
#' @return Object 'fit' stripped of components that are not required to make predictions.
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
#' predict(fit_light, newdata = dat)
#' @export
trim_glm_fit <- caret::getModelInfo("glm")$glm$trim


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
  fit$type_of_preds <- algorithm$type_of_preds
  return(fit)
}

#' @export
compute_lGbar_hatAW <- function(A, W, Gbar_hat, threshold = 0.05) {
  dat <- data.frame(A = A, W = W)
  GW <- predict(Gbar_hat, newdata = dat, type = Gbar_hat$type_of_preds)
  lGAW <- A * GW + (1 - A) * (1 - GW)
  pred <- pmin(1 - threshold, pmax(lGAW, threshold))
  return(pred)
}

#' @export
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

#' @export
working_model_G_three <- list(
  model = function(...) {trim_glm_fit(glm(family = binomial(), ...))},
  formula = as.formula(
    paste("A ~",
          paste("I(", c("cos", "sin", "sqrt", "log", "exp"), sep = "", collapse = "(W)) + "),
          "(W))")
  ),
  type_of_preds = "response"
)
attr(working_model_G_three, "ML") <- FALSE
(working_model_G_three$formula)

#' @export
estimate_Qbar <- function(dat, algorithm, ...) {
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

#' @export
compute_Qbar_hatAW <- function(Y, A, W, Qbar_hat, blip = FALSE) {
  if (!blip) {
    dat <- data.frame(Y = Y, A = A, W = W)
    pred <- predict(Qbar_hat, newdata = dat, type = Qbar_hat$type_of_preds)
  } else {
    pred <- predict(Qbar_hat, newdata = data.frame(A = 1, W = W),
                    type = Qbar_hat$type_of_preds) -
      predict(Qbar_hat, newdata = data.frame(A = 0, W = W),
              type = Qbar_hat$type_of_preds)
  }
  return(pred)  
}

#' @export
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


#' @export
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
#' @export
kknn_grid <- expand.grid(kmax = 5, distance = 2, kernel = "gaussian")
#' @export
control <- caret::trainControl(method = "cv", number = 2,
                               predictionBounds = c(0, 1),
                               trim = TRUE,
                               allowParallel = TRUE)






#' @export
estimate_QW <- function(dat) {
  dat %>% as.tibble %>% select(value = W) %>%
    mutate(weight = 1/n())
}


#' @export
wrapper <- function(fit) {
  pryr::unenclose(function(obs) {
    obs <- as.data.frame(obs)
    predict(fit, newdata = obs, type = fit$type_of_preds)
  })
}

