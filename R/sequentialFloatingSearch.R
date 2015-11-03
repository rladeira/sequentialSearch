
sequentialFloatingSearch <-
  function(attributes,
           evaluationFunction,
           type = c("SFFS", "SFBE"),
           verbose = TRUE,
           isHigherBetter = TRUE,
           ...) {

    if (is.function(evaluationFunction) == FALSE)
      stop("evaluationFunction must be a function.")
    if (is.vector(attributes) == FALSE)
      stop("attributes must a vector.")

    type <- match.arg(type)

    if (type == "SFFS") {
      currentAttrEncoding <- rep(0, length(attributes))
      firstStep <- createSequentialSearchStepFunc(type = "SFS")
      firstStepDescription <- "Inclusion"
      conditionalStep <- createSequentialSearchStepFunc(type = "SBE")
      conditionalStepDescription <- "Exclusion"
    }
    else { # type == "SFBE"
      currentAttrEncoding <- rep(1, length(attributes))
      firstStep <-  createSequentialSearchStepFunc(type = "SBE")
      firstStepDescription <- "Exclusion"
      conditionalStep <- createSequentialSearchStepFunc(type = "SFS")
      conditionalStepDescription <- "Inclusion"
    }

    trace <- list()
    iteration <- 1
    traceIndex <- 1

    initialAttrSubset <- attributes[as.logical(currentAttrEncoding)]
    bestScoreSoFar <- evaluationFunction(initialAttrSubset, ...)

    while(TRUE) {

      # ----------------------- Step 1 ------------------------------
      # ---------------- Inclusion for SFFS -------------------------
      # ---------------- Exclusion for SFBE  -------------------------

      if (verbose)
        message("Iteration: ", iteration,
                "\n             ",  firstStepDescription, " Step ",
                " | Current Encoding: ", paste(currentAttrEncoding, collapse = ""),
                " | Optimization Value: ", round(bestScoreSoFar, 4))

      trace[[traceIndex]] <-
        list(attrEncoding = currentAttrEncoding,
             optimizationValue = bestScoreSoFar)

      iteration <- iteration + 1
      traceIndex <- traceIndex + 1

      firstStepResult <-
        firstStep(
          attributes,
          currentAttrEncoding,
          bestScoreSoFar,
          evaluationFunction,
          verbose = verbose,
          isHigherBetter = isHigherBetter,
          ...)

      if (firstStepResult$isFinalStep) {

        indexes <- as.logical(firstStepResult$finalAttrEncoding)
        solution <- attributes[indexes]

        return(list(
          solution = solution,
          trace = trace))
      }

      currentAttrEncoding <- firstStepResult$nextAttrEncoding
      bestScoreSoFar <- firstStepResult$bestScore

      # ----------------------- Step 2 ------------------------------
      # ----------- Conditional Exclusion for SFFS -------------------------
      # ----------- Conditional Inclusion for SFBE  -------------------------

      if (verbose)
        message(" Conditional ", conditionalStepDescription, " Step ",
                " | Current Encoding: ", paste(currentAttrEncoding, collapse = ""),
                " | Optimization Value: ", round(bestScoreSoFar, 4),
                "\n-----------------------------------------------------------------------------")

      trace[[traceIndex]] <-
        list(attrEncoding = currentAttrEncoding,
             optimizationValue = bestScoreSoFar)

      traceIndex <- traceIndex + 1

      conditionalStepResult <-
        conditionalStep(
          attributes,
          currentAttrEncoding,
          bestScoreSoFar,
          evaluationFunction,
          verbose = verbose,
          isHigherBetter = isHigherBetter,
          ...)

      if (conditionalStepResult$bestScore < bestScoreSoFar ||
          conditionalStepResult$isFinalStep)
        next

      bestScoreSoFar <- conditionalStepResult$bestScore
      currentAttrEncoding <- conditionalStepResult$nextAttrEncoding
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
