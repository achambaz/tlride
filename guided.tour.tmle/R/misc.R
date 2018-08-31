#' Convenient functions
#' 
#' @docType data
#'
#' @format \code{Functions}.
#'
#' @references Benkeser & Chambaz, "A Guided Tour in Targeted Learning Territory" (2018).  
#' 
#' @examples
#'
#' label(1:17, c(2, 3))
#'
#' @name convenient_functions
NULL

#' @docType data
#' @rdname convenient_functions
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
#' @references Benkeser & Chambaz, "A Guided Tour in Targeted Learning Territory" (2018).
#' 
#' @examples
#'
#' sample_from_mixture_of_uniforms(7, c(1/3, 2/3), mins = c(0, 1/2), maxs = c(2/3, 3/4))
#'
"sample_from_mixture_of_uniforms"
