sample_from_mixture_of_uniforms <-
  function(n, mixture_weights, mins, maxs) {
    ## preliminary
    n <- Arguments$getInteger(n, c(1, Inf))
    mixture_weights <- Arguments$getNumerics(mixture_weights, c(0, Inf))
    mins <- Arguments$getNumerics(mins)
    maxs <- Arguments$getNumerics(maxs)
    if (!(length(mixture_weights) == length(mins) &
          length(mins) == length(maxs))) {
      stop(str_c("Arguments 'mixture_weights', 'mins' and 'maxs' ",
                 "must have the same length.\n"))
    }
    if (!sum(mixture_weights) == 1) {
      stop("The entries of 'mixture_weights' must sum up to one.\n")
    }
    if (!all(mins <= maxs)) {
      stop("Caution: 'mins[i]' must be smaller than 'maxs[i]' for all 'i'.\n")
    }
    ##
    latent <- findInterval(runif(n), cumsum(mixture_weights)) + 1
    W <- runif(n, min = mins[latent], max = maxs[latent])
    return(W)
  }

sigma0 <- function(obs) {
  ## preliminary
  Qbar <- .get_feature(another_experiment, "Qbar", h = 0)
  QAW <- Qbar(obs[, c("A", "W")])
  params <- formals(.get_feature(another_experiment, "qY", h = 0))
  shape1 <- eval(params$shape1)
  ## computations
  betaAW <- shape1 * (1 - QAW) / QAW
  out <- log(1 - obs[, "Y"])
  for (int in 1:shape1) {
    out <- out + 1/(int - 1 + betaAW)
  }
  out <- - out * shape1 * (1 - QAW) / QAW * 10 * sqrt(obs[, "W"]) * obs[, "A"]
  ## no need to center given how we will use it
  return(out)
}

label <- function(xx, sample_size = c(1e3, 2e3)) {
  by <- sum(sample_size)
  xx <- xx[seq_len((length(xx) %/% by) * by)] - 1
  prefix <- xx %/% by
  suffix <- findInterval(xx %% by, cumsum(sample_size))
  paste(prefix + 1, suffix + 1, sep = "_")
}
