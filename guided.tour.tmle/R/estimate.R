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
#' predict(fit_light, newdata = dat)
#'
#' @export trim_glm_fit
trim_glm_fit <- caret::getModelInfo("glm")$glm$trim

#' Does this
#'
#' Does this.
#' 
#' @param dat ...
#'
#' @param algorithm ...
#'
#' @param ... Additional parameters passed to the algorithm.
#'
#' @seealso \code{\link{estimate_QW}}, \code{\link{estimate_Qbar}}
#'
#' @return something
#'
#' @examples
#'
#' ## do this and that
#' @export estimate_Gbar
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


#' Does this
#'
#' Does this.
#' 
#' @param dat ...
#'
#' @param algorithm ...
#'
#' @param ... Additional parameters passed to the algorithm.
#'
#' @seealso \code{\link{estimate_QW}}, \code{\link{estimate_Gbar}}
#'
#' @return something
#'
#' @examples
#'
#' ## do this and that (refer to working models and knn)
#'
#' @export estimate_Qbar
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

#' Estimates the marginal law of W non-parametrically
#'
#' Estimates the marginal law of W non-parametrically.
#' 
#' @param dat ...
#'
#' @seealso \code{\link{estimate_Gbar}}, \code{\link{estimate_Qbar}}
#'
#' @return A  \code{tibble}  with  columns named  'value'  and 'weight'  that
#'   describes the empirical law of 'W' in data set 'dat'.
#'
#' @examples
#' 
#' ## do this and that
#' @export estimate_QW
estimate_QW <- function(dat) {
  dat %>% tibble::as.tibble %>%
    dplyr::select(value = "W") %>%
    dplyr::mutate(weight = 1/dplyr::n())
}
