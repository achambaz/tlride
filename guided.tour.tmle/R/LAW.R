R.oo::setConstructorS3("LAW", function(Gbar = NA, Qbar = NA, QW = NA,
                                       qY= NA, sample_from = NA) {
  R.oo::extend(R.oo::Object(), "LAW",
               .QW = QW,
               .Gbar = Gbar,
               .Qbar = Qbar,
               .qY = qY,
               .sample_from = sample_from)
})


#' Prints an object of class LAW
#'
#' Prints an object of class LAW.
#'
#' @name as.character
#' @param x An object of class \code{LAW}.
#' @param \dots Not used.
#' @return A character string recalling what can be done with an object of class \code{LAW}.
#' @aliases as.character.LAW as.character
#' @export as.character
R.methodsS3::setMethodS3(
  "as.character", "LAW",
  function(x, ...) {
    this <- x ## to please R CMD check
    msg <- stringr::str_c("A law. If it is fully characterized, you can use method ",
                          "'sample_from' to sample from it. ",
                          "If you built it, or if you are an _oracle_, ",
                          " you can also use methods ",
                          "'reveal' to reveal some of its relevant features, ",
                          "'update' to change one of its features, ",
                          "'evaluate_psi' to obtain the value of 'Psi' at this law, ",
                          "'evaluate_eic' to obtain the efficient influence curve of ",
                          "'Psi' at this law.") %>%
      stringr::str_wrap(indent = 2, width = 60) %>%
      stringr::str_c("\n")
    class(msg) <- "GenericSummary"
    return(msg)
  }, private = TRUE)

#' Samples from an object of class LAW.
#'
#' Samples from an object of class \code{LAW} when it is not underdetermined.
#'
#' @name sample_from
#' @param this An object of class \code{LAW}
#' @param n An \code{integer}, the wished sample size. Defaults to 'n = 1'.
#' @param \dots Additional parameters possibly needed to fully determine the law.
#' @aliases sample_from.LAW sample_from 
#' @seealso \code{\link{reveal}}, \code{\link{update}}, \code{\link{evaluate_psi}}, \code{\link{evaluate_eic}}
#' @return Either \code{NULL} if the law is undetermined or the data set sampled, a \code{tibble} or \code{data.frame} with columns named 'W', 'A' and 'Y'.   
#' @export sample_from
R.methodsS3::setMethodS3(
  "sample_from", "LAW",
  function(this, n = 1, ...) {
  n <- R.utils::Arguments$getInteger(n, c(1, Inf))
  is_na <- suppressWarnings(is.na(this$.sample_from))
  if (is_na) {
    message(stringr::str_c("Cannot sample from _underdetermined_ law '",
                           deparse(substitute(this)),"'."))
    out <- NULL
  } else{
    out <- this$.sample_from(n, ...)
  }
    return(out)
})

#' Reveals an object of class LAW
#'
#' Reveals an object of class \code{LAW} by exhibiting a selection of relevant features. Can be performed if one has built the object or if one acts as an oracle.
#'
#' @name reveal
#' @param this An object of class \code{LAW}
#' @param \dots  Additional parameters possibly needed to  fully determine the
#'   law.
#' @aliases reveal.LAW reveal
#'       @seealso       \code{\link{sample_from}},       \code{\link{update}},
#'   \code{\link{evaluate_psi}}, \code{\link{evaluate_eic}}
#' @return A \code{list} with tags\itemize{\item  'QW', marginal law of 'W', a
#'   \code{function}  (the  density) or  a  \code{tibble}  with columns  named
#'   'value'   and  'weight'   (a  discrete   law)\item  'Gbar',   conditional
#'   probability  that  'A=1'  given  'W',  a  \code{function}  \item  'Qbar',
#'   conditional  mean of  'Y' given  '(A,W)', a  \code{function} \item  'qY',
#'   conditional density  of 'Y' given  '(A,W)', a \code{function}.}   Each of
#'   them equals 'NA' if it is not determined.
#' @export reveal
R.methodsS3::setMethodS3("reveal", "LAW", function(this, ...) {
  list(QW = this$.QW,
       Gbar = this$.Gbar,
       Qbar = this$.Qbar,
       qY = this$.qY)       
})

R.methodsS3::setMethodS3(".get_feature", "LAW", function(this, what, ...){
  if (!(what %in% c("Qbar", "Gbar", "QW", "qY"))) {
    stop(stringr::str_c("Argument 'what' should be one of 'Qbar', 'Gbar', 'QW', 'qY', not ",
                        deparse(substitute(what)),
                        "\n"))
  }
  some_relevant_features <- reveal(this)
  ellipsis <- list(...)
  feature <- eval(substitute(some_relevant_features$a, list(a = what)))
  if (what == "QW" & !is.function(feature)) {
    if (length(setdiff(c("value", "weight"), names(feature))) > 0) {
      stop(stringr::str_c("Argument 'QW' of law '",
                          deparse(substitute(this)),
                          "' is not a function. Is it well characterized as",
                          " a 'matrix', 'tibble' or 'data.frame' with columns",
                          " named 'value' and 'weight'?\n") %>%
           stringr::str_wrap(indent = 2, width = 60) %>%
           stringr::str_c("\n"))
    } 
  } else {
    len <- ifelse(what == "qY", 2, 1)
    if (length(formals(feature)) > len) {
      params <- formals(feature)[-(1:len)]
      params <- params[sapply(params, is.symbol)]
      if (length(params) > 0) {
        if (!all(names(params) %in% names(ellipsis))) {
          stop(stringr::str_c("Is law '", deparse(substitute(this)),
                              "' fully characterized?\n"))
        } else {
          idx <- which(names(ellipsis) %in% names(params))
          feature <-
            ifelse(what == "qY",
                   function(X, Y) {
                     feat <- eval(substitute(some_relevant_features$a, list(a = what)))     
                     do.call(feat, c(ellipsis[idx], list(X, Y)))
                   },
                   function(X) {
                     feat <- eval(substitute(some_relevant_features$a, list(a = what)))     
                     do.call(feat, c(ellipsis[idx], list(X)))
                   })
        }
      }
    }
  }
  return(feature)
})

#' @export evaluate_psi
R.methodsS3::setMethodS3("evaluate_psi", "LAW", function(this, ...) {
  some_relevant_features <- reveal(this)
  ellipsis <- list(...)
  if (length(intersect(names(some_relevant_features), c("Qbar", "QW"))) != 2) {
    stop(stringr::str_c("Is law '", deparse(substitute(this)),
                        "' an element of the model where 'Psi' is defined?\n"))
  } else {
    Qbar <- .get_feature(this, "Qbar", ...)
    QW <- .get_feature(this, "QW", ...)
    if (is.function(QW)) {
      integrand <- function(w) {
        ( Qbar(cbind(A = 1, W = w)) - Qbar(cbind(A = 0, W = w)) ) * QW(w)
      }
      out <- integrate(integrand, lower = 0, upper = 1)$val
    } else {
      if (!identical(names(QW), c("value", "weight"))) {
        stop(stringr::str_c("Argument 'QW' is neither a function nor a valid",
                            " discrete law.\n"))
      }
      W <- dplyr::pull(QW, value)
      out <- Qbar(cbind(A = 1, W = W)) - Qbar(cbind(A = 0, W = W))
      out <- weighted.mean(out, pull(QW, weight))
    }
    return(out)
  }
})


#' @export evaluate_eic
R.methodsS3::setMethodS3("evaluate_eic", "LAW", function(this, psi = NULL, ...) {
  if (is.null(psi)) {
    psi <- evaluate_psi(this, ...)
  }
  some_relevant_features <- reveal(this)
  ellipsis <- list(...)
  if (length(intersect(names(some_relevant_features), c("Qbar", "QW"))) != 2) {
    stop(stringr::str_c("Is law '", deparse(substitute(this)),
                        "' an element of the model where 'Psi' is defined?\n"))
  } else {
    Qbar <- .get_feature(this, "Qbar", ...)
    Gbar <- .get_feature(this, "Gbar", ...)
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
      out <- (QoneW - QzeroW - psi) + (2 * obs[, "A"] - 1) / lGAW * (obs[, "Y"] - QAW)
      out <- as.vector(out)
      return(out)
    }
    return(eic)
  }
})

#' @export update
R.methodsS3::setMethodS3("update", "LAW", function(this, ...) {
  sys_call <- as.list(sys.call())
  ellipsis <- list(...)
  valid <- c("Qbar", "Gbar", "QW", "qY", "sample_from")
  invalid <- setdiff(names(ellipsis), valid)
  if(length(ellipsis) == 0) {
    stop("Must specify which feature to update.")
  }
  if (length(invalid) > 0) {
    stop(stringr::str_c("Can update one among '",
                        stringr::str_flatten(valid, "', '"),
                        "' not '",
                        stringr::str_flatten(invalid, "', '"),
               "'.", sep = ""))
  } else {
    what <- names(ellipsis)
    for (ii in 1:length(ellipsis)) {
      value <- eval(sys_call[[what[ii]]])
      eval(parse(text = paste0("this$.", what[ii], " <- value")))
    }
  }
})
