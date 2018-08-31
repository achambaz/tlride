sigma0 <- function(obs) {
  ## preliminary
  law <- guided.tour.tmle:::another_experiment
  Qbar <- get_feature(law, "Qbar", h = 0)
  QAW <- Qbar(obs[, c("A", "W")])
  params <- formals(get_feature(law, "qY", h = 0))
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
