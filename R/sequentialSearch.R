
sequentialSearch <-
  function(attributes,
           evaluationFunction,
           type = c("SFS", "SBE"),
           verbose = TRUE,
           isHigherBetter = TRUE,
           ...) {

    if (is.function(evaluationFunction) == FALSE)
      stop("evaluationFunction must be a function.")
    if (is.vector(attributes) == FALSE)
      stop("attributes must a vector.")

    type <- match.arg(type)

    if (type == "SFS")
      currentAttrEncoding <- rep(0, length(attributes))
    else # type == "SBE"
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
          bestScoreSoFar, evaluationFunction,
          type, verbose, isHigherBetter, ...)

      if (stepResult$isFinalStep) {
        solution <-
          attributes[as.logical(stepResult$finalAttrEncoding)]

        return(list(
          solution = solution,
          trace = trace))
      }

      currentAttrEncoding <- stepResult$nextAttrEncoding
      bestScoreSoFar <- stepResult$bestScore
    }

    stop("Should never reach this line...")
  }
