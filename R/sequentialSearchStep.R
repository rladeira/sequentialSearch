
sequentialSearchStep <-
  function(attributes,
           currentAttrEncoding,
           evaluationFunction,
           type = c("SFS", "SBS"),
           ...) {

    type <- match.arg(type)

    if (type == "SFS")
      neighbors <- forwardNeighbors(currentAttrEncoding)
    else # type == "SBS"
      neighbors <- backwardNeighbors(currentAttrEncoding)

    neighborsScores <-
      getNeighborsEvaluation(
        attributes, neighbors,
        evaluationFunction, ...)

    bestNeighborsOrdering <-
      order(neighborsScores, decreasing = TRUE)

    return(list(
      neighborsOrderedByScore = neighbors[bestNeighborsOrdering],
      orderedScores = neighborsScores[bestNeighborsOrdering]))
  }
