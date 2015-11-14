
getNeighborsEvaluation <- function(attributes,
                                   neighbors,
                                   evaluationFunction,
                                   allowParallel = TRUE,
                                   ...) {

  if (!require(foreach))
    stop("Please install foreach: install.packages('foreach')")

  if (allowParallel) {
    neighborsEvaluations <-
      foreach(n = neighbors,
              .combine = "c") %dopar% {

                subsetIndexes <- as.logical(n)
                subset <- attributes[subsetIndexes]
                evaluationFunction(subset, ...)
              }
  }
  else {
    neighborsEvaluations <-
      foreach(n = neighbors,
              .combine = "c") %do% {

                subsetIndexes <- as.logical(n)
                subset <- attributes[subsetIndexes]
                evaluationFunction(subset, ...)
              }
  }

  if (is.null(neighborsEvaluations))
    neighborsEvaluations <- numeric()

  return(neighborsEvaluations)
}
