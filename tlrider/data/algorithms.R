working_model_G_one <- list(
  model = function(...) {trim_glm_fit(glm(family = binomial(), ...))},
  formula = stats::as.formula(
    paste("A ~",
          paste(c("I(W^", "I(abs(W - 5/12)^"),
                rep(seq(1/2, 3/2, by = 1/2), each = 2),
                sep = "", collapse = ") + "),
          ")")
  ),
  type_of_preds = "response"
)
attr(working_model_G_one, "ML") <- FALSE

working_model_G_two <- list(
  model = function(...) {trim_glm_fit(glm(family = binomial(), ...))},
  formula = stats::as.formula(
    paste("A ~",
          paste(c("I(W^", "I(abs(W - 5/12)^"),
                rep(seq(1/4, 3, by = 1/4), each = 2), ## make sure '1/2' and '1' belong to 'powers'
                sep = "", collapse = ") + "),
          ")")
  ),
  type_of_preds = "response"
)
attr(working_model_G_two, "ML") <- FALSE

working_model_G_three <- list(
  model = function(...) {trim_glm_fit(glm(family = binomial(), ...))},
  formula = stats::as.formula(
    paste("A ~",
          paste("I(", c("cos", "sin", "sqrt", "log", "exp"), sep = "", collapse = "(W)) + "),
          "(W))")
  ),
  type_of_preds = "response"
)
attr(working_model_G_three, "ML") <- FALSE


working_model_Q_one <- list(
  model = function(...) {trim_glm_fit(glm(family = binomial(), ...))},
  formula = stats::as.formula(
    paste("Y ~ A * (",
          paste("I(W^", seq(1/2, 3/2, by = 1/2), sep = "", collapse = ") + "),
          "))")
  ),
  type_of_preds = "response"
)
attr(working_model_Q_one, "ML") <- FALSE


kknn_algo <- list(
  algo = function(dat, ...) {
    args <- list(...)
    if ("Subsample" %in% names(args)) {
      keep <- sample.int(nrow(dat), args$Subsample)
      dat <- dat[keep, ]
    }
    fit <- caret::train(Y ~ I(10*A) + W, ## a tweak
                        data = dat,
                        method = "kknn",
                        verbose = FALSE,
                        ...)
    fit$finalModel$fitted.values <- NULL
    ## nms <- names(fit$finalModel$data)
    ## for (ii in match(setdiff(nms, ".outcome"), nms)) {
    ##   fit$finalModel$data[[ii]] <- NULL
    ## }
    fit$trainingData <- NULL    
    return(fit)
  },
  type_of_preds = "raw"
)
attr(kknn_algo, "ML") <- TRUE

kknn_grid <- expand.grid(kmax = 5, distance = 2, kernel = "gaussian")

kknn_control <- caret::trainControl(method = "cv", number = 2,
                                    predictionBounds = c(0, 1),
                                    trim = TRUE,
                                    allowParallel = TRUE)
