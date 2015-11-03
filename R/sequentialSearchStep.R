
sequentialSearchStep <-
  function(attributes,
           currentAttrEncoding,
           bestScoreSoFar,
           evaluationFunction,
           type = c("SFS", "SBE"),
           isHigherBetter = TRUE,
           ...) {

    type <- match.arg(type)

    if (type == "SFS")
      neighbors <- forwardNeighbors(currentAttrEncoding)
    else # type == "SBE"
      neighbors <- backwardNeighbors(currentAttrEncoding)

    if (length(neighbors) == 0)
      return(list(
        isFinalStep = TRUE,
        bestScore = bestScoreSoFar,
        finalAttrEncoding = currentAttrEncoding
      ))

    neighborsEvaluations <-
      getNeighborsEvaluation(
        attributes, neighbors,
        evaluationFunction, ...)

    # Test whether or not the current encoding is the best found.

    bestNeighborEvaluation <- max(neighborsEvaluations)
    bestNeighborIndex <- which.max(neighborsEvaluations)

    if (bestScoreSoFar >= bestNeighborEvaluation)
      return(list(
        isFinalStep = TRUE,
        bestScore = bestScoreSoFar,
        finalAttrEncoding = currentAttrEncoding
      ))

    # There's a better solution than the current.
    bestScoreSoFar <- bestNeighborEvaluation
    nextAttrEncoding <- neighbors[[bestNeighborIndex]]

    return(list(
      isFinalStep = FALSE,
      bestScore = bestScoreSoFar,
      nextAttrEncoding = nextAttrEncoding
    ))
  }
