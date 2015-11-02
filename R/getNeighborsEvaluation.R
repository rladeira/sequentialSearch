
getNeighborsEvaluation <-
  function(attributes, neighbors, evaluationFunction, ...) {

    if (!require(foreach))
      stop("Please install foreach: install.packages('foreach')")

    neighborsEvaluations <-
      foreach(n = neighbors,
              .combine = "c") %dopar% {
                subsetIndexes <- as.logical(n)
                subset <- attributes[subsetIndexes]
                evaluationFunction(subset, ...)
              }

    return(neighborsEvaluations)
  }
