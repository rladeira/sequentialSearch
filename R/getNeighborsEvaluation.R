
getNeighborsEvaluation <- 
  function(neighbors, evaluationFunction, ...) {
    
    if (!require(foreach))
      stop("Please install foreach: install.packages('foreach')")
    
    neighborsEvaluations <-
      foreach(n = neighbors,
              .combine = "c") %dopar% {
                evaluationFunction(n, ...)
              }
    
    return(neighborsEvaluations)
  }
