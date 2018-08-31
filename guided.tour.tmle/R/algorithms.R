#' Algorithms for the estimation of Gbar and Qbar
#'
#' Algorithms  for the  estimation of  'Gbar' and  'Qbar'. Some  are based  on
#' parametric  working  models  and  the  \code{\link{glm}}  procedure  (their
#' \code{attribute}  'ML' is  set to  \code{FALSE}). Another  is based  on the
#' k-nearest  neighbors  non-parametric  procedure as  implement  in  'method'
#' \code{kknn} of  the 'caret'  package (its \code{attribute}  'ML' is  set to
#' \code{TRUE}).
#' 
#' @docType data
#'
#' @usage data(guided.tour.tmle)
#'
#' @format  Either  a  \code{list}   with  entries  'model',  'formula'  and
#'   'type_of_preds'  (with \code{attribue}  'ML'  set to  \code{FALSE}) or  a
#'   \code{list} with entries 'algo' and 'type_of_preds' (with \code{attribue}
#'   'ML' set to \code{TRUE}).
#'
#' @references Benkeser & Chambaz, "A Guided Tour in Targeted Learning Territory" (2018).  
#' 
#' @examples
#'
#' (working_model_G_one)
#' (kknn_algo)
#'
#' @name algorithms
NULL

#' @docType data
#' @rdname algorithms
"working_model_G_one"

#' @docType data
#' @rdname algorithms
"working_model_G_two"

#' @docType data
#' @rdname algorithms
"working_model_G_three"

#' @docType data
#' @rdname algorithms
"working_model_Q_one"

#' @docType data
#' @rdname algorithms
"kknn_algo"

#' @docType data
#' @rdname algorithms
"kknn_grid"

#' @docType data
#' @rdname algorithms
"kknn_control"

#' Convenient functions.
#'
#' Convenient functions.
#' 
#' @docType data
#'
#' @usage data(guided.tour.tmle)
#'
#' @format A \code{function}.
#'
#' @references Benkeser & Chambaz, "A Guided Tour in Targeted Learning Territory" (2018).  
#' 
#' @name convenient_functions
NULL

#' @docType data
#' @rdname convenient_functions
"compute_lGbar_hatAW"

#' @docType data
#' @rdname convenient_functions
"compute_Qbar_hatAW"

#' @docType data
#' @rdname convenient_functions
"wrapper"

