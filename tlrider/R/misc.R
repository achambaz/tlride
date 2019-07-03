#' Labels the elements of a data set
#'
#' Labels the elements of a data set to prepare subsamples.
#' 
#' @docType data
#'
#' @param xx A \code{vector}.
#'
#' @param sample_size A \code{vector} of \code{integer}s.
#'
#' @return A \code{vector} of \code{characters}.
#'
#' @details Let 'L'  be the length of 'sample_size' and  'Q'be the quotient in
#'   the Euclidean division of the  length of 'xx' by 'sum(sample_size)'.  The
#'   \code{function} \code{label}  creates 'Q  x L'  different strings  of the
#'   form 'q_l'.   Each 'q_l' string  is repeated 'sample_size[l]'  times. The
#'   function is used to make easily sub-data sets of a data set.
#'
#' @references Benkeser & Chambaz, "A Ride in Targeted Learning Territory" (2019).
"label"

#' Samples from a mixture of uniform laws
#'
#' Samples  from the  mixture of  uniform laws  characterized by  the provided
#' weights and corresponding intervals.
#'
#' @docType data
#'
#' @param n An \code{integer}, the sample size.
#'
#' @param  mixture_weights A  \code{vector} of \code{numerics}  that specifies
#'   the weights given to the uniform laws. Must sum up to one.
#'
#' @param  mins  A  \code{vector}   of  \code{numerics}  that  specifies  the
#'   lower-bounds of the supports of the uniform laws.
#'
#' @param  maxs  A  \code{vector}   of  \code{numerics}  that  specifies  the
#'   upper-bounds of the supports of the uniform laws.
#'
#' @return A \code{vector} of numerics, the sample.
#'
#' @references Benkeser & Chambaz, "A Ride in Targeted Learning Territory" (2019).
"sample_from_mixture_of_uniforms"

#' Fluctuates a Qbar feature of an object of class LAW
#'
#' Fluctuates a  Qbar feature  \eqn{\bar{Q}} of  an object of  class LAW  in a
#' direction characterized  by a  Gbar feature \eqn{\bar{G}}  of an  object of
#' class  LAW.   Specifically,  for  any  real  number  \eqn{h},  \deqn{\logit
#' (\bar{Q}_h(A,W)) =  \logit (\bar{Q}_h(A,W)) + h  \frac{2A-1}{A \bar{G}(W) +
#' (1-A) (1 - \bar{G}(W))}.}
#'
#' @param   Qbar  A   conditional  expectation  of   'Y'  given   '(A,W)',  a
#'   \code{function}.  Must  have  the  same  form  as  the  Qbar  feature  of
#'   'experiment' (see '?get_feature').
#'
#' @param   Gbar  A  conditional   probability  that  'A=1'  given   'W',  a
#'   \code{function}. Must  have  the  same  form  as  the  Gbar  feature  of
#'   'experiment' (see '?get_feature').
#'
#' @param h A \code{numeric}
#'
#' @return A \code{function}.
#'
#' @references Benkeser & Chambaz, "A Ride in Targeted Learning Territory" (2019).
#'
#' @seealso   \code{\link{estimate_Qbar}}   to  estimate   the   conditional
#'   expectation of Y given (A,W), \code{\link{estimate_Gbar}} to estimate the
#'   conditional probability that A=1 given (A,W), \code{\link{wrapper}}.
#'
#' @examples
#'
#' ## create an experiment, sample from it
#' example(tlrider, echo = FALSE)
#' experiment
#' obs <- sample_from(experiment, n = 250)
#'
#' ## extract its Gbar and Qbar features
#' Gbar <- get_feature(experiment, "Gbar")
#' Qbar <- get_feature(experiment, "Qbar")
#'
#' ## fluctuate Qbar using Gbar
#' Q_h <- fluctuate(Qbar, Gbar, h = 1/2)
#' Q_h(cbind(A=0:1, W = c(1/3, 2/3)))
#'
#' ## estimate 'Gbar' and 'Qbar'
#' Gbar_hat <- estimate_Gbar(obs, working_model_G_one)
#' Qbar_hat <- estimate_Qbar(obs, working_model_Q_one)
#'
#' ## wrap 'Gbar_hat' and 'Qbar_hat' (two fits) into two functions
#' Gbar_hat_fun <- wrapper(Gbar_hat, FALSE)
#' Qbar_hat_fun <- wrapper(Qbar_hat, FALSE)
#' 
#' ## fluctuate Qbar_hat_fun using Gbar_hat_fun
#' Qhat_h <- fluctuate(Qbar_hat_fun, Gbar_hat_fun, h = 1/2)
#' Qhat_h(cbind(A=0:1, W = c(1/3, 2/3)))
#' 
#' @export
fluctuate <- function(Qbar, Gbar, h) {
  function(AW) {
    A <- AW[, "A", drop = FALSE]
    W <- AW[, "W", drop = FALSE]
    lGAW <- A * Gbar(W) + (1 - A) * (1 - Gbar(W))
    out <- stats::plogis(stats::qlogis(Qbar(AW)) + h * (2 * A - 1) / lGAW)
    return(as.vector(out))
  }
}

