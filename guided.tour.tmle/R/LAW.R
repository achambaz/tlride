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
#'
#' @param x An object of class \code{LAW}.
#' 
#' @param \dots Not used.
#' 
#' @return  A character string  recalling what can be  done with an  object of
#'   class \code{LAW}.
#' 
#' @aliases as.character.LAW as.character
#' 
#' @export as.character
#' 
#' @importFrom magrittr %>%
R.methodsS3::setMethodS3(
  "as.character", "LAW",
  function(x, ...) {
    this <- x ## to please R CMD check
    msg <-  
      stringr::str_c("A law for (W,A,Y) in [0,1] x {0,1} x [0,1].\n",
                     stringr::str_wrap(
                       "If the law is fully characterized, you can use method ",
                       "'sample_from' to sample from it.\n",
                       indent = 2, width = 60),
                     stringr::str_wrap(
                       "If you built the law, or if you are an _oracle_, ",
                       " you can also use methods ",
                       "'reveal' to reveal its relevant features ",
                       "(QW, Gbar, Qbar, qY -- see '?reveal') ",
                       "and 'alter' to change some of them.",
                       indent = 2, width = 60),
                     stringr::str_wrap(
                       "If all its relevant features are characterized, you can ",
                       "use methods 'evaluate_psi' to obtain the value of 'Psi' ",
                       " at this law (see '?evaluate_psi') and 'evaluate_eic' to ",
                       " obtain the efficient influence curve of 'Psi' at this law ",
                       "(see '?evaluate_eic').",
                       indent = 2, width = 60),
                     "\n")
    class(msg) <- "GenericSummary"
    return(msg)
  }, private = TRUE)

#' Samples from an object of class LAW
#'
#' Samples data of the form (W,A,Y) in [0,1] x {0,1} x [0,1] from an object of
#' class \code{LAW} when it fully characterizes a law.
#'
#' @name sample_from
#' 
#' @param this An object of class \code{LAW}.
#' 
#' @param n An \code{integer}, the wished sample size. Defaults to 'n = 1'.
#' 
#' @param  \dots Additional parameters  possibly needed to  fully characterize
#'   the law.
#' 
#' @aliases sample_from.LAW sample_from
#' 
#'         @seealso         \code{\link{reveal}},         \code{\link{alter}},
#'   \code{\link{evaluate_psi}}, \code{\link{evaluate_eic}}
#' 
#' @return Either  \code{NULL} if  the law  is undetermined  or the  data set
#'   sampled, a \code{tibble} or \code{data.frame} with columns named 'W', 'A'
#'   and 'Y'.
#' 
#' @export sample_from
R.methodsS3::setMethodS3(
  "sample_from", "LAW",
  function(this, n = 1, ...) {
  n <- R.utils::Arguments$getInteger(n, c(1, Inf))
  is_na <- suppressWarnings(is.na(this$.sample_from))
  if (is_na) {
    message(stringr::str_c("Cannot sample from an _underdetermined_ law '",
                           deparse(substitute(this)),"'."))
    out <- NULL
  } else{
    out <- this$.sample_from(n, ...)
  }
    return(out)
})

#' Reveals an object of class LAW
#'
#' Reveals an object of class \code{LAW} by exhibiting a selection of relevant
#' features of the  law for (W,A,Y) in  [0,1] x {0,1} x [0,1]  that the object
#' characterizes. Can be performed if one has  built the object or if one acts
#' as an \emph{oracle}.
#'
#' @name reveal
#'
#' @param this An object of class \code{LAW}.
#'
#' @param  \dots Additional parameters  possibly needed to  fully characterize
#'   the law.
#'
#' @aliases reveal.LAW reveal
#'
#' @seealso        \code{\link{sample_from}},       \code{\link{alter}},
#'   \code{\link{evaluate_psi}}, \code{\link{evaluate_eic}}
#'
#' @return A \code{list} with tags\itemize{\item  'QW', marginal law of 'W', a
#'   \code{function}  (the  density) or  a  \code{tibble}  with columns  named
#'   'value'   and  'weight'   (a  discrete   law)\item  'Gbar',   conditional
#'   probability  that  'A=1'  given  'W',  a  \code{function}  \item  'Qbar',
#'   conditional  mean of  'Y' given  '(A,W)', a  \code{function} \item  'qY',
#'   conditional density  of 'Y' given  '(A,W)', a \code{function}.}   Each of
#'   them equals 'NA' if it is not characterized.
#'
#' @export reveal
R.methodsS3::setMethodS3("reveal", "LAW", function(this, ...) {
  list(QW = this$.QW,
       Gbar = this$.Gbar,
       Qbar = this$.Qbar,
       qY = this$.qY)       
})

R.methodsS3::setMethodS3("get_feature", "LAW", function(this, what, ...){
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

#' Evaluates the statistical mapping Psi at an object of class LAW
#'
#' Evaluates at the law for (W,A,Y) in  [0,1] x {0,1} x [0,1] characterized by
#' an object of  class \code{LAW} the statistical mapping  \eqn{\Psi} given by
#' \deqn{\Psi(P) = E_P {Qbar(1,W)  - Qbar(0,W)}} where \eqn{Qbar(A,W)
#' = E_P (Y|A,W)}. Can be performed if one has built the object or if one acts
#' as an  \emph{oracle}.  Works only if  all the relevant features  of the law
#' are characterized.
#'
#' @name evaluate_psi
#'
#' @param this An object of class \code{LAW}.
#'
#' @param \dots  Additional parameters possibly needed to  fully characterize the
#'   law.
#'
#' @aliases evaluate_psi.LAW evaluate_psi
#'
#' @seealso       \code{\link{sample_from}},       \code{\link{reveal}},
#'   \code{\link{alter}}, \code{\link{evaluate_eic}}
#'
#' @return  A \code{function},  the efficient  infuence curve  of statistical
#'   mapping \eqn{\Psi} evaluated at the law described by the \code{LAW} object
#'   \code{this}.
#'
#' @export evaluate_psi
R.methodsS3::setMethodS3("evaluate_psi", "LAW", function(this, ...) {
  some_relevant_features <- reveal(this)
  ellipsis <- list(...)
  if (length(intersect(names(some_relevant_features), c("Qbar", "QW"))) != 2) {
    stop(stringr::str_c("Is law '", deparse(substitute(this)),
                        "' an element of the model where 'Psi' is defined?\n"))
  } else {
    Qbar <- get_feature(this, "Qbar", ...)
    QW <- get_feature(this, "QW", ...)
    if (is.function(QW)) {
      integrand <- function(w) {
        ( Qbar(cbind(A = 1, W = w)) - Qbar(cbind(A = 0, W = w)) ) * QW(w)
      }
      out <- stats::integrate(integrand, lower = 0, upper = 1)$val
    } else {
      if (!identical(names(QW), c("value", "weight"))) {
        stop(stringr::str_c("Argument 'QW' is neither a function nor a valid",
                            " discrete law.\n"))
      }
      W <- dplyr::pull(QW, "value")
      out <- Qbar(cbind(A = 1, W = W)) - Qbar(cbind(A = 0, W = W))
      out <- stats::weighted.mean(out, dplyr::pull(QW, "weight"))
    }
    return(out)
  }
})


#' Evaluates the efficient influence curve of Psi at an object of class LAW
#'
#' Evaluates  at   efficient  influence  curve  of   the  statistical  mapping
#' \eqn{\Psi} at the law for (W,A,Y) in [0,1] x {0,1} x [0,1] characterized by
#' an object  of class \code{LAW}.   The mapping \eqn{\Psi} and  its efficient
#' influence  curve  at  a  law  \eqn{P} are  given  by  \deqn{\Psi(P)  =  E_P
#' {Qbar(1,W) -  Qbar(0,W)}} with \eqn{Qbar(A,W)  = E_P (Y|A,W)}  and \eqn{D^*
#' (P)  = D_1^*  (P) +  D_2^*(P)}  where \deqn{D_1^*(P)(W,A,Y)  = Qbar(1,W)  -
#' Qbar(0,W) - \Psi(P)} and  \deqn{D_2^*(P)(W,A,Y) = \frac{2A-1}{Gbar(A,W)} (Y
#' - Qbar(A,W)).}
#'
#' The evaluation can be performed if one  has built the object or if one acts
#' as an  \emph{oracle}.  Works only if  all the relevant features  of the law
#' are characterized.
#'
#' @name evaluate_eic
#'
#' @param this An object of class \code{LAW}.
#'
#' @param \dots  Additional parameters possibly needed to  fully characterize the
#'   law.
#'
#' @aliases evaluate_eic.LAW evaluate_eic
#'
#' @seealso       \code{\link{sample_from}},       \code{\link{reveal}},
#'   \code{\link{alter}}, \code{\link{evaluate_psi}}
#'
#' @return  A \code{function},  the efficient  infuence curve  of statistical
#'   mapping \eqn{\Psi} evaluated at the law described by the \code{LAW} object
#'   \code{this}.
#'
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
    Qbar <- get_feature(this, "Qbar", ...)
    Gbar <- get_feature(this, "Gbar", ...)
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


#' Alters an object of class LAW
#'
#' Alters an object of class \code{LAW} by ...
#'
#' @name alter 
#'
#' @param this An object of class \code{LAW}.
#'
#' @param \dots ...
#'
#' @aliases alter.LAW alter
#'
#' @seealso       \code{\link{sample_from}},       \code{\link{reveal}},
#'   \code{\link{evaluate_psi}}, \code{\link{evaluate_eic}}
#'
#' @return something
#'
#' @export alter
R.methodsS3::setMethodS3("alter", "LAW", function(this, ...) {
  sys_call <- as.list(sys.call())
  ellipsis <- list(...)
  valid <- c("Qbar", "Gbar", "QW", "qY", "sample_from")
  invalid <- setdiff(names(ellipsis), valid)
  if(length(ellipsis) == 0) {
    stop("Must specify which feature to alter.")
  }
  if (length(invalid) > 0) {
    stop(stringr::str_c("Can alter one among '",
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
