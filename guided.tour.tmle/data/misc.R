label <- function(xx, sample_size = c(1e3, 2e3)) {
  xx <- R.utils::Arguments$getVector(xx)
  sample_size <- R.utils::Arguments$getIntegers(sample_size)
  by <- sum(sample_size)
  if (length(xx) < by) {
    stop("The sum of argument 'sample_size' is larger than the length of argument 'xx'.\n")
  }
  xx <- xx[seq_len((length(xx) %/% by) * by)] - 1
  prefix <- xx %/% by
  suffix <- findInterval(xx %% by, cumsum(sample_size))
  paste(prefix + 1, suffix + 1, sep = "_")
}

sample_from_mixture_of_uniforms <-
  function(n, mixture_weights, mins, maxs) {
    ## preliminary
    n <- R.utils::Arguments$getInteger(n, c(1, Inf))
    mixture_weights <- R.utils::Arguments$getNumerics(mixture_weights, c(0, Inf))
    mins <- R.utils::Arguments$getNumerics(mins)
    maxs <- R.utils::Arguments$getNumerics(maxs)
    if (!(length(mixture_weights) == length(mins) &
          length(mins) == length(maxs))) {
      stop(stringr::str_c("Arguments 'mixture_weights', 'mins' and 'maxs' ",
                          "must have the same length.\n"))
    }
    if (!sum(mixture_weights) == 1) {
      stop("The entries of 'mixture_weights' must sum up to one.\n")
    }
    if (!all(mins <= maxs)) {
      stop("Caution: 'mins[i]' must be smaller than 'maxs[i]' for all 'i'.\n")
    }
    ##
    latent <- findInterval(stats::runif(n), cumsum(mixture_weights)) + 1
    W <- stats::runif(n, min = mins[latent], max = maxs[latent])
    return(W)
  }

