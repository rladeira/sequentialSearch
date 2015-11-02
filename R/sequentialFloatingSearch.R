
sequentialFloatingSearch <-
  function(attributes,
           evaluationFunction,
           type = c("SFFS", "SFBE"),
           traceExecution = TRUE,
           verbose = TRUE,
           isHigherBetter = TRUE,
           ...) {

    if(is.function(evaluationFunction) == FALSE)
      stop("evaluationFunction must be a function.")
    if(is.vector(attributes) == FALSE)
      stop("attributes must a vector.")

    type <- match.arg(type)

    if (type == "SFFS") {
      currentAttrEncoding <- rep(0, length(attributes))
      inclusionStep <-
        createSequentialSearchStepFunc(type = "SFS")
      exclusionStep <-
        createSequentialSearchStepFunc(type = "SBE")
    }
    else { # type == "SFBE"
      currentAttrEncoding <- rep(1, length(attributes))
      inclusionStep <-
        createSequentialSearchStepFunc(type = "SBE")
      exclusionStep <-
        createSequentialSearchStepFunc(type = "SFS")
    }

    if (traceExecution) {
      trace <- list()
      iteration <- 1
      traceIndex <- 1
    }

    initialAttrSubset <- attributes[as.logical(currentAttrEncoding)]
    bestScoreSoFar <- evaluationFunction(initialAttrSubset, ...)

    while(TRUE) {

      # ------------ Step 1: Inclusion -------------------------

      if (traceExecution) {
        if (verbose)
          message("Iteration: ", iteration,
                  "\n             Inclusion Step ",
                  " | Start Encoding: ", paste(currentAttrEncoding, collapse = ""),
                  " | Optimization Value: ", bestScoreSoFar)

        trace[[traceIndex]] <-
          list(attrEncoding = currentAttrEncoding,
               optimizationValue = bestScoreSoFar)

        iteration <- iteration + 1
        traceIndex <- traceIndex + 1
      }

      inclusionStepResult <-
        inclusionStep(
          attributes,
          currentAttrEncoding,
          bestScoreSoFar,
          evaluationFunction,
          verbose = verbose,
          isHigherBetter = isHigherBetter,
          ...)

      if (inclusionStepResult$isFinalStep) {

        indexes <- as.logical(inclusionStepResult$finalAttrEncoding)
        solution <- attributes[indexes]

        if (traceExecution)
          return(list(
            solution = solution,
            trace = trace))
        else
          return(solution)
      }

      currentAttrEncoding <- inclusionStepResult$nextAttrEncoding
      bestScoreSoFar <- inclusionStepResult$bestScore

      # ------------ Step 2: Conditional Exclusion ---------------

      if (traceExecution) {
        if (verbose)
          message(" Conditional Exclusion Step ",
                  " | Start Encoding: ", paste(currentAttrEncoding, collapse = ""),
                  " | Optimization Value: ", bestScoreSoFar,
                  "\n-----------------------------------------------------------------------------")

        trace[[traceIndex]] <-
          list(attrEncoding = currentAttrEncoding,
               optimizationValue = bestScoreSoFar)

        traceIndex <- traceIndex + 1
      }

      exclusionStepResult <-
        exclusionStep(
          attributes,
          currentAttrEncoding,
          bestScoreSoFar,
          evaluationFunction,
          verbose = verbose,
          isHigherBetter = isHigherBetter,
          ...)

      if (exclusionStepResult$bestScore < bestScoreSoFar ||
          exclusionStepResult$isFinalStep)
        next

      bestScoreSoFar <- exclusionStepResult$bestScore
      currentAttrEncoding <- exclusionStepResult$nextAttrEncoding
    }

    stop("Should never reach this line...")
  }

createSequentialSearchStepFunc <-
  function(type = c("SFS", "SBE")) {

    type <- match.arg(type)

    stepFunc <-
      function(currentAttrEncoding, bestScoreSoFar,
               evaluationFunction,
               verbose = verbose,
               isHigherBetter = isHigherBetter,
               ...) {

        sequentialSearchStep(
          currentAttrEncoding, bestScoreSoFar,
          evaluationFunction,
          type = type,
          verbose = verbose,
          isHigherBetter = isHigherBetter,
          ...)
      }

    return(stepFunc)
  }
