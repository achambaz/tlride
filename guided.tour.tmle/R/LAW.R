#' Initializes an object of class LAW
#'
#' Initializes  an object  of \code{class}  \code{LAW}. The  object can  fully
#' characterize  a law  for (W,A,Y)  in [0,1]  x {0,1}  x [0,1],  ie, make  it
#' possible to  sample from it. A  collection of features of  special interest
#' can be specified.
#'
#' @name LAW
#'
#' @param  QW The marginal  law of 'W', a  \code{function} (the density)  or a
#'   \code{tibble} with columns named 'value' and 'weight' (a discrete law).
#' 
#' @param  Gbar   The  conditional  probability  that  'A=1'   given  'W',  a
#'   \code{function}.
#'
#' @param Qbar The conditional  mean of  'Y' given  '(A,W)', a  \code{function}.
#'
#' @param qY The conditional density of 'Y' given '(A,W)', a \code{function}
#'
#' @param sample_from A \code{function}  to sample from the law.
#'
#' @return Returns an [R.oo::Object] of \code{class} LAW.
#'
#' @examples
#'
#' ## See 'example(guided.tour.tmle)'
#' 
#' @family methods and functions for LAW objects
#'
#' @export 
setConstructorS3("LAW", function(QW = NA, Gbar = NA, Qbar = NA, 
                                 qY= NA, sample_from = NA) {
  extend(Object(), "LAW",
         .QW = QW,
         .Gbar = Gbar,
         .Qbar = Qbar,
         .qY = qY,
         .sample_from = sample_from)
})


#' Copies an object of class LAW
#'
#' Given an  object of class  \code{LAW}, makes a copy  of it in  another such
#' object.
#'
#' @name copy
#' 
#' @param this An object of \code{class} \code{LAW}.
#'
#' @param ... Not used.
#'
#' @family methods and functions for LAW objects
#' 
#' @return A copy of argument \code{this}.
#'
#' @examples
#'
#' ## create an experiment, make a copy (once)
#' example(guided.tour.tmle, echo = FALSE)
#' experiment_bis <- experiment
#' experiment_ter <- copy(experiment)
#' identical(experiment, experiment_bis)
#' identical(experiment, experiment_ter)
#' 
#' @references Benkeser & Chambaz, "A Guided Tour in Targeted Learning Territory" (2018).
#'
#' @aliases copy
#' 
#' @export copy
#' 
#' @export
setMethodS3(
  "copy", "LAW", function(this, ...) {
    LAW(Gbar = this$.Gbar,
        Qbar = this$.Qbar,
        QW = this$.QW,
        qY = this$.qY,
        sample_from = this$.sample_from)
  })



#' Prints an object of class LAW
#'
#' Prints an object of \code{class} \code{LAW}.
#'
#' @name as.character
#'
#' @param x An object of \code{class} \code{LAW}.
#' 
#' @param \dots Not used.
#' 
#' @return  A character string  recalling what can be  done with an  object of
#'   \code{class} \code{LAW}.
#' 
#' @references Benkeser & Chambaz, "A Guided Tour in Targeted Learning Territory" (2018).
#'
#' @family methods and functions for LAW objects
#'
#' @export
setMethodS3(
  "as.character", "LAW",
  function(x, ...) {
    this <- x ## to please R CMD check
    width <- floor(0.8 * getOption("width"))
    msg <-
      stringr::str_c(
        "A law for (W,A,Y) in [0,1] x {0,1} x [0,1].",
        stringr::str_wrap("  If the law is fully characterized, you can use method
                       'sample_from' to sample from it.",
                       indent = 0, width = width, exdent = 0),
        stringr::str_wrap("If you built the law, or if you are an _oracle_,
                       you can also use methods
                       'reveal' to reveal its relevant features
                       (QW, Gbar, Qbar, qY -- see '?reveal'),
                       and 'alter' to change some of them.",
                       indent = 0, width = width, exdent = 0),
        stringr::str_wrap("If all its relevant features are characterized, you can
                       use methods 'evaluate_psi' to obtain the value of 'Psi'
                       at this law (see '?evaluate_psi') and 'evaluate_eic' to
                       obtain the efficient influence curve of 'Psi' at this law
                       (see '?evaluate_eic').",
                       indent = 0, width = width, exdent = 0), sep = "\n\n")
    class(msg) <- "GenericSummary"
    return(msg)
  }, private = TRUE)

#' Samples from an object of class LAW
#'
#' Samples data of the form (W,A,Y) in [0,1] x {0,1} x [0,1] from an object of
#' \code{class} \code{LAW} when it fully characterizes a law.
#'
#' @name sample_from
#' 
#' @param this An object of \code{class} \code{LAW}.
#' 
#' @param n An \code{integer}, the wished sample size. Defaults to 'n = 1'.
#' 
#' @param  \dots Additional parameters  possibly needed to  fully characterize
#'   the law.
#' 
#' @seealso \code{\link{reveal}} to reveal  some relevant features of the law,
#'   \code{\link{alter}}   to  modify   them,  \code{\link{evaluate_psi}}   to
#'   evaluate    the     value    of    \eqn{Psi}    at     the    law,    and
#'   \code{\link{evaluate_eic}} to  evaluate the efficient influence  curve of
#'   \eqn{Psi} at the law.
#'
#' @family methods and functions for LAW objects
#' 
#' @return Either  \code{NULL} if  the law  is undetermined  or the  data set
#'   sampled, a \code{tibble} or \code{data.frame} with columns named 'W', 'A'
#'   and 'Y'.
#'
#' @references Benkeser & Chambaz, "A Guided Tour in Targeted Learning Territory" (2018).
#'
#' @examples
#'
#' ## create an experiment
#' example(guided.tour.tmle, echo = FALSE)
#' experiment
#'
#' ## sample three observations from it
#' sample_from(experiment, n = 3)
#'
#' @aliases sample_from
#' 
#' @export sample_from
#' 
#' @export
setMethodS3(
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
#' Reveals an object  of \code{class} \code{LAW} by exhibiting  a selection of
#' relevant features of the law for (W,A,Y)  in [0,1] x {0,1} x [0,1] that the
#' object characterizes.  Can be performed if  one has built the  object or if
#' one acts as an \emph{oracle}.
#'
#' @name reveal
#'
#' @param this An object of \code{class} \code{LAW}.
#'
#' @param  \dots Additional parameters  possibly needed to  fully characterize
#'   the law.
#'
#' @family methods and functions for LAW objects
#' 
#' @references Benkeser & Chambaz, "A Guided Tour in Targeted Learning Territory" (2018).
#'
#' @seealso \code{\link{sample_from}}  to sample from the law (if  it is fully
#'   characterized), \code{\link{alter}}  to modify some relevant  features of
#'   the law, \code{\link{evaluate_psi}} to evaluate the value of \eqn{Psi} at
#'   the  law,  and  \code{\link{evaluate_eic}}   to  evaluate  the  efficient
#'   influence curve of \eqn{Psi} at the law.
#'
#' @examples
#'
#' ## create then reveal an experiment
#' example(guided.tour.tmle, echo = FALSE)
#' reveal(experiment)
#' 
#' 
#' @return A \code{list} with tags\itemize{\item  'QW', marginal law of 'W', a
#'   \code{function}  (the  density) or  a  \code{tibble}  with columns  named
#'   'value'   and  'weight'   (a  discrete   law)\item  'Gbar',   conditional
#'   probability  that  'A=1'  given  'W',  a  \code{function}  \item  'Qbar',
#'   conditional  mean of  'Y' given  '(A,W)', a  \code{function} \item  'qY',
#'   conditional  density  of  'Y'  given  '(A,W)',  a  \code{function}  \item
#'   'sample_from', a \code{function}  to sample from the law.}   Each of them
#'   equals 'NA' if it is not characterized.
#'
#' @aliases reveal
#' 
#' @export reveal
#' 
#' @export
setMethodS3(
  "reveal", "LAW", function(this, ...) {
  list(QW = this$.QW,
       Gbar = this$.Gbar,
       Qbar = this$.Qbar,
       qY = this$.qY,
       sample_from = this$.sample_from)       
})

#' Reveals one feature of an object of class LAW
#'
#' Reveals  one  feature   of  an  object  of   \code{class}  \code{LAW}  that
#' characterizes a law for (W,A,Y) in [0,1]  x {0,1} x [0,1]. Can be performed
#' if one has built the object or if one acts as an \emph{oracle}.
#'
#' @name get_feature
#'
#' @param this An object of \code{class} \code{LAW}.
#'
#' @param what A \code{character}, one among "Qbar", "Gbar", "QW", "qY".
#'
#' @param  \dots Additional parameters  possibly needed to  fully characterize
#'   the law.
#'
#' @family methods and functions for LAW objects
#' 
#' @references Benkeser & Chambaz, "A Guided Tour in Targeted Learning Territory" (2018).
#'
#' @seealso \code{\link{reveal}} to reveal  some relevant features of the law.
#'
#' @examples
#'
#' ## create then reveal an experiment
#' example(guided.tour.tmle, echo = FALSE)
#' get_feature(experiment, "Gbar")
#' 
#' 
#'@return  Depending on  argument  'what',  one among  \itemize{\item  'QW',
#'   marginal law of  'W', a \code{function} (the density)  or a \code{tibble}
#'   with columns  named 'value'  and 'weight'  (a discrete  law)\item 'Gbar',
#'   conditional  probability that  'A=1' given  'W', a  \code{function} \item
#'   'Qbar', conditional  mean of 'Y'  given '(A,W)', a  \code{function} \item
#'   'qY', conditional density of 'Y' given '(A,W)', a \code{function}.}  Each
#'   of them equals 'NA' if it is not characterized in object 'this'.
#'
#' @aliases get_feature
#' 
#' @export get_feature
#'
#' @export
setMethodS3(
  "get_feature", "LAW", function(this, what, ...){
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
      width <- floor(0.8 * getOption("width"))
      stop(stringr::str_c("Argument 'QW' of law '",
                          deparse(substitute(this)),
                          "' is not a function. Is it well characterized as",
                          " a 'matrix', 'tibble' or 'data.frame' with columns",
                          " named 'value' and 'weight'?\n") %>%
           stringr::str_wrap(indent = 0, width = width, exdent = 0) %>%
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
#' an object of  \code{class} \code{LAW} the statistical mapping  \eqn{\Psi} given by
#' \deqn{\Psi(P) = E_P {Qbar(1,W)  - Qbar(0,W)}} where \eqn{Qbar(A,W)
#' = E_P (Y|A,W)}. Can be performed if one has built the object or if one acts
#' as an  \emph{oracle}.  Works only if  all the relevant features  of the law
#' are characterized.
#'
#' @name evaluate_psi
#'
#' @param this An object of \code{class} \code{LAW}.
#'
#' @param \dots  Additional parameters possibly needed to  fully characterize the
#'   law.
#'
#' @references Benkeser & Chambaz, "A Guided Tour in Targeted Learning Territory" (2018).
#' 
#' @seealso \code{\link{sample_from}}  to sample from the law (if  it is fully
#'   characterized), \code{\link{reveal}} to reveal  some relevant features of
#'   the law, \code{\link{alter}} to modify some relevant features of the law,
#'   and \code{\link{evaluate_eic}} to evaluate  the efficient influence curve
#'   of \eqn{Psi} at the law.
#'
#' @family methods and functions for LAW objects
#' 
#' @return  A \code{function},  the efficient  infuence curve  of statistical
#'   mapping \eqn{\Psi} evaluated at the law described by the \code{LAW} object
#'   \code{this}.
#'
#' @examples
#'
#' ## create an experiment
#' example(guided.tour.tmle, echo = FALSE)
#' experiment
#'
#' ## evaluate Psi at this experiment
#' evaluate_psi(experiment)
#'
#' @aliases evaluate_psi
#' 
#' @export evaluate_psi
#'
#' @export
setMethodS3(
  "evaluate_psi", "LAW", function(this, ...) {
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
#' an  object of  \code{class}  \code{LAW}.  The  mapping  \eqn{\Psi} and  its
#' efficient influence curve at a law \eqn{P} are given by \deqn{\Psi(P) = E_P
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
#' @param this An object of \code{class} \code{LAW}.
#'
#' @param  psi A  \code{numeric},  the  value of  \eqn{Psi}  at  the law.  If
#'   \code{NULL} (default), then it is computed by calling \code{evaluate_psi}. 
#' 
#' @param \dots  Additional parameters possibly needed to  fully characterize the
#'   law.
#' 
#' @references Benkeser & Chambaz, "A Guided Tour in Targeted Learning Territory" (2018).
#' 
#' @seealso \code{\link{sample_from}}  to sample from the law (if  it is fully
#'   characterized), \code{\link{reveal}} to reveal  some relevant features of
#'   the law, \code{\link{alter}} to modify some relevant features of the law,
#'   and \code{\link{evaluate_psi}} to evaluate the  value of \eqn{Psi} at the
#'   law.
#'
#' @family methods and functions for LAW objects
#' 
#' @return  A \code{function},  the efficient  infuence curve  of statistical
#'   mapping \eqn{\Psi} evaluated at the law described by the \code{LAW} object
#'   \code{this}.
#'
#' @examples
#'
#' ## create an experiment
#' example(guided.tour.tmle, echo = FALSE)
#' experiment
#'
#' ## evaluate the efficient influence curve of Psi at this experiment
#' eic <- evaluate_eic(experiment)
#'
#' ## sample five observations from the experiment, then evaluate 'eic' at them
#' obs <- sample_from(experiment, n = 5)
#' eic(obs)
#'
#' @aliases evaluate_eic
#'
#' @export evaluate_eic
#' 
#' @export
setMethodS3(
  "evaluate_eic", "LAW", function(this, psi = NULL, ...) {
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
#' Alters  an object  of \code{class} \code{LAW} by  changing some  of the  relevant
#' features of the  law for (W,A,Y) in  [0,1] x {0,1} x [0,1]  that the object
#' characterizes. Can be performed if one has built the object.
#'
#' Specifically,  the relevant  features  can be  chosen among  \itemize{\item
#' 'QW',  the marginal  law  of 'W',  a \code{function}  (the  density), or  a
#' \code{tibble} with columns named 'value' and 'weight' (a discrete law)\item
#' 'Gbar', the conditional probability that 'A=1' given 'W', a \code{function}
#' \item 'Qbar', the conditional mean  of 'Y' given '(A,W)', a \code{function}
#' \item 'qY', the conditional density of 'Y' given '(A,W)', a \code{function}
#' \item 'sample_from', a \code{function} to sample from the law.} Setting any
#' of them to \code{NA} \emph{uncharacterizes} it.
#' 
#' Caution should be exercised, since changing some features of the object may
#' put the \code{sample_from} method at odds with the modified features.
#' 
#' @name alter 
#'
#' @param this An object of \code{class} \code{LAW}.
#'
#' @param \dots ...
#'
#' @references Benkeser & Chambaz, "A Guided Tour in Targeted Learning Territory" (2018).
#' 
#' @seealso \code{\link{sample_from}}  to sample from the law (if  it is fully
#'   characterized), \code{\link{reveal}} to reveal  some relevant features of
#'   the law, \code{\link{evaluate_psi}} to evaluate the value of \eqn{Psi} at
#'   the  law,  and  \code{\link{evaluate_eic}}   to  evaluate  the  efficient
#'   influence curve of \eqn{Psi} at the law.
#'
#' @family methods and functions for LAW objects
#' 
#' @return An object of \code{class} \code{LAW}, with modified features.
#'
#' @examples
#'
#' ## create an experiment
#' example(guided.tour.tmle, echo = FALSE)
#'
#' ## reveal the original experiment, alert and reveal it again
#' reveal(experiment)
#' alter(experiment, Gbar = function(W){rep_len(1/2, length(W))})
#' reveal(experiment)
#'
#' @aliases alter
#'
#' @export alter
#' 
#' @export
setMethodS3(
  "alter", "LAW", function(this, ...) {
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

#' Evaluates the remainder term at a law w.r.t. another law
#' 
#' Consider two laws \eqn{P} and \eqn{P'} for (W,A,Y) in [0,1] x {0,1} x [0,1]
#' characterized by  two objects of  \code{class} \code{LAW}. Recall  that the
#' mapping \eqn{\Psi} and  its efficient influence curve at  \eqn{P} are given
#' by \deqn{\Psi(P) =  E_P {Qbar(1,W) - Qbar(0,W)}} with  \eqn{Qbar(A,W) = E_P
#' (Y|A,W)}   and   \eqn{D^*    (P)   =   D_1^*   (P)    +   D_2^*(P)}   where
#' \deqn{D_1^*(P)(W,A,Y)   =   Qbar(1,W)   -    Qbar(0,W)   -   \Psi(P)}   and
#' \deqn{D_2^*(P)(W,A,Y) = \frac{2A-1}{Gbar(A,W)} (Y - Qbar(A,W)).}
#'
#' This  function evaluates  the remainder  term at  \eqn{P'} w.r.t.  \eqn{P}:
#' \deqn{Rem_P(P') = \Psi(P') - \Psi(P) - (P - P')D^*(P').}
#'
#' The evaluation can be performed if one  has built the two objects or if one
#' acts as an  \emph{oracle}.  Works only if all the  relevant features of the
#' laws are characterized.
#'
#' @name evaluate_remainder
#'
#' @param this An object of \code{class} \code{LAW} describing, say, \eqn{P}.
#'
#' @param that An object of \code{class} \code{LAW} describing, say, \eqn{P'}.
#'
#' @param params If not  \code{NULL} (default value), a \code{list} consisting
#'   of  two  \code{list}s,  each specifying  additional  parameters  possibly
#'   needed to fully characterize the corresponding law.
#'
#' @references Benkeser & Chambaz, "A Guided Tour in Targeted Learning Territory" (2018).
#' 
#' @seealso  \code{\link{evaluate_psi}} to  evaluate \eqn{Psi}  at a  law and
#'   \code{\link{evaluate_eic}} to  evaluate the efficient influence  curve of
#'   \eqn{Psi} at a law.
#' 
#' @return A  \code{numeric},  the value of the remainder term \eqn{Rem_P(P')}.
#'
#' @family methods and functions for LAW objects
#' 
#' @examples
#'
#' ## create a one-dimensional fluctutation (collection of laws)
#' example(guided.tour.tmle, echo = FALSE)
#' (evaluate_psi(another_experiment, h = 0)) # Psi(P_0)
#' (evaluate_psi(another_experiment, h = 1)) # Psi(P_1)
#' 
#' ## evaluate the remainder term
#' ## (hint: same 'Gbar' feature, so ...)
#' evaluate_remainder(another_experiment, another_experiment,
#'                    list(list(h = 0), list(h = 1)))
#'
#' ## create another one-dimensional fluctuation
#' yet_another_experiment <- copy(another_experiment)
#' alter(yet_another_experiment, Gbar = function(w){rep_len(1/2, length(w))})
#'
#' ## evaluate the remainder term
#' evaluate_remainder(another_experiment, yet_another_experiment,
#'                    list(list(h = 0), list(h = 1)))
#'
#' @aliases evaluate_remainder
#'
#' @export evaluate_remainder
#' 
#' @export
evaluate_remainder <- function(this, that, params = NULL) {
  if (!is.null(params)) {
    if (!length(params) == 2) {
      width <- floor(0.8 * getOption("width"))
      stop(stringr::str_wrap("  If not 'NULL', argument 'params'
                              must be a list consisting of two lists,
                              each one providing parameters to the
                              corresponding object of class 'LAW'.
                              If one of the objects needs no such parameters,
                              then the corresponding list should be 'list()'.",
                             indent = 0, width = width, exdent = 0))
    } else {
      params_A <- params[[1]]
      params_B <- params[[2]]
    }
  }
  some_relevant_features_A <- reveal(this)
  some_relevant_features_B <- reveal(that)
  if (length(intersect(names(some_relevant_features_A), c("Gbar", "Qbar", "QW"))) != 3) {
    stop(stringr::str_wrap(
      stringr::str_c("  The 'QW', 'Gbar' and 'Qbar' features of law'",
                     deparse(substitute(this)),
                     "' must be characterized."),
      indent = 0, width = width, exdent = 0))
  } else if (length(intersect(names(some_relevant_features_B), c("Gbar", "Qbar"))) != 2) {
    stop(stringr::str_wrap(
      stringr::str_c("  The 'Gbar' and 'Qbar' features of law'",
                     deparse(substitute(that)),
                     "' must be characterized."),
      indent = 0, width = width, exdent = 0))    
  } else {
    Gbar_A <- do.call(get_feature, c(list(this = this, what = "Gbar"), params_A))
    Qbar_A <- do.call(get_feature, c(list(this = this, what = "Qbar"), params_A))
    QW_A <- do.call(get_feature, c(list(this = this, what = "QW"), params_A))
    Gbar_B <- do.call(get_feature, c(list(this = that, what = "Gbar"), params_B))
    Qbar_B <- do.call(get_feature, c(list(this = that, what = "Qbar"), params_B))
    if (is.function(QW_A)) {
      integrand <- function(w) {
        fw <- ( Qbar_A(cbind(A = 1, W = w)) - Qbar_B(cbind(A = 1, W = w)) ) / Gbar_B(w) 
        fw <- fw +
          ( Qbar_A(cbind(A = 0, W = w)) - Qbar_B(cbind(A = 0, W = w)) ) / (1 - Gbar_B(w))
        fw <- fw * (Gbar_A(w) - Gbar_B(w)) * QW_A(w)
        return(fw)
      }
      out <- stats::integrate(integrand, lower = 0, upper = 1)$val
    } else {
      if (!identical(names(QW_A), c("value", "weight"))) {
        stop(stringr::str_c("Argument 'QW' is neither a function nor a valid", " discrete law.\n"))
      }
      W <- dplyr::pull(QW_A, "value")
      fw <- ( Qbar_A(cbind(A = 1, W = W)) - Qbar_B(cbind(A = 1, W = W)) ) / Gbar_B(W) 
      fw <- fw +
        ( Qbar_A(cbind(A = 0, W = W)) - Qbar_B(cbind(A = 0, W = W)) ) / (1 - Gbar_B(W))
      fw <- fw * (Gbar_A(W) - Gbar_B(W))
      out <- stats::weighted.mean(fw, dplyr::pull(QW_A, "weight"))
    }
    return(out)
  }
}
