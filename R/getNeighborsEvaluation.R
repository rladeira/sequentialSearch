
library(foreach)

getNeighborsEvaluation <- 
  function(neighbors, evaluationFunction, ...) {
    
    neighborsEvaluations <-
      foreach(n = neighbors,
              .combine = "c") %dopar% {
                evaluationFunction(n, ...)
              }
    
    return(neighborsEvaluations)
  }
