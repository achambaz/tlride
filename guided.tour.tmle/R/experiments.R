#' Built-in examples of objects of class LAW
#'
#' Built-in  objects \code{experiment}  and  \code{another_experiment} are  of
#' class \code{LAW}.   The corresponding laws for  (W,A,Y) in [0,1] x  {0,1} x
#' [0,1] are fully characterized, up to  the specification of a fine-tune real
#' parameter   for   \code{another_experiment}   which  actually   defines   a
#' one-dimensional parametric model (a \emph{fluctuation}).
#'
#' Built-in \code{function}  \code{sigma0} implements the  \emph{direction} of
#' the fluctuation characterized by \code{another_experiment}.
#' 
#' @docType data
#'
#' @format Two objects of class \code{LAW} and a \code{function}.
#'
#' @references Benkeser & Chambaz, "A Guided Tour in Targeted Learning Territory" (2018).  
#' 
#' @examples
#'
#' experiment
#'
#' another_experiment
#'
#' sigma0
#'
#' @name experiments
NULL

#' @docType data
#' @rdname experiments
"sigma0"
