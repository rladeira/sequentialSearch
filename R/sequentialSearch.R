
sequentialSearch <- function(attributes,
                             evaluationFunction,
                             type = c("sfs", "sbs"),
                             verbose = TRUE,
                             allowParallel = TRUE,
                             ...) {

  if (is.function(evaluationFunction) == FALSE)
    stop("evaluationFunction must be a function.")
  if (is.vector(attributes) == FALSE)
    stop("attributes must a vector.")

  type <- match.arg(type)

  if (type == "sfs")
    currentAttrEncoding <- rep(0, length(attributes))
  else # type == "sbs"
    currentAttrEncoding <- rep(1, length(attributes))

  trace <- list()
  traceIndex <- 1

  initialAttrSubset <- attributes[as.logical(currentAttrEncoding)]
  bestScoreSoFar <- evaluationFunction(initialAttrSubset, ...)

  while (TRUE) {

    if (verbose)
      message("Iteration: ", traceIndex,
              " | Current encoding: ", paste(currentAttrEncoding, collapse = ""),
              " | Optimization value: ", round(bestScoreSoFar, 4))

    trace[[traceIndex]] <-
      list(attrEncoding = currentAttrEncoding,
           optimizationValue = bestScoreSoFar)

    traceIndex <- traceIndex + 1

    stepResult <-
      sequentialSearchStep(
        attributes, currentAttrEncoding,
        evaluationFunction, type, ...)


    # If there's no more neighbors to visit or the best
    # score from the neighbors is not good enough, the
    # current best solution is returned
    if (length(stepResult$neighborsOrderedByScore) == 0 ||
        bestScoreSoFar >= stepResult$orderedScores[1]) {

      solution <- attributes[as.logical(currentAttrEncoding)]

      return(list(
        solution = solution,
        trace = trace))
    }

    # Update the current best solution
    currentAttrEncoding <- stepResult$neighborsOrderedByScore[[1]]
    bestScoreSoFar <- stepResult$orderedScores[1]
  }

  stop("Should never reach this line...")
}
