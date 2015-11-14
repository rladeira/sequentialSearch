
sequentialSearchStep <- function(attributes,
                                 currentAttrEncoding,
                                 evaluationFunction,
                                 type = c("sfs", "sbs"),
                                 allowParallel = TRUE,
                                 ...) {

  type <- match.arg(type)

  if (type == "sfs")
    neighbors <- forwardNeighbors(currentAttrEncoding)
  else # type == "sbs"
    neighbors <- backwardNeighbors(currentAttrEncoding)

  neighborsScores <- getNeighborsEvaluation(
    attributes, neighbors,
    evaluationFunction,
    allowParallel, ...)

  bestNeighborsOrdering <-
    order(neighborsScores, decreasing = TRUE)

  return(list(
    neighborsOrderedByScore = neighbors[bestNeighborsOrdering],
    orderedScores = neighborsScores[bestNeighborsOrdering]))
}
