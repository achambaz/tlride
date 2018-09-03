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
#' @references Benkeser & Chambaz, "A Guided Tour in Targeted Learning Territory" (2018).
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
"sample_from_mixture_of_uniforms"
