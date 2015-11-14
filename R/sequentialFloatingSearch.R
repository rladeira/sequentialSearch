
sequentialFloatingSearch <- function(attributes,
                                     evaluationFunction,
                                     type = c("sffs", "sfbs"),
                                     verbose = TRUE,
                                     allowParallel = TRUE,
                                     ...) {

  if (is.function(evaluationFunction) == FALSE)
    stop("evaluationFunction must be a function.")
  if (is.vector(attributes) == FALSE)
    stop("attributes must a vector.")

  type <- match.arg(type)

  ## Starts with the empty set of attributes
  if (type == "sffs") {
    currentAttrEncoding <- rep(0, length(attributes))
    firstStep <- createSequentialSearchStepFunc(type = "sfs")
    firstStepDescription <- "Inclusion"
    conditionalStep <- createSequentialSearchStepFunc(type = "sbs")
    conditionalStepDescription <- "Exclusion"
  }
  ## Starts with the full set of attributes
  else { # type == "sfbs"
    currentAttrEncoding <- rep(1, length(attributes))
    firstStep <-  createSequentialSearchStepFunc(type = "sbs")
    firstStepDescription <- "Exclusion"
    conditionalStep <- createSequentialSearchStepFunc(type = "sfs")
    conditionalStepDescription <- "Inclusion"
  }

  trace <- list()
  iteration <- 1
  traceIndex <- 1

  initialAttrSubset <- attributes[as.logical(currentAttrEncoding)]
  bestScoreSoFar <- evaluationFunction(initialAttrSubset, ...)

  while(TRUE) {

    # ----------------------- Step 1 ------------------------------
    # ---------------- Inclusion for sffs -------------------------
    # ---------------- Exclusion for sfbs  -------------------------

    if (verbose)
      message("Iteration: ", iteration,
              "\n             ",  firstStepDescription, " Step ",
              " | Current Encoding: ", paste(currentAttrEncoding, collapse = ""),
              " | Optimization Value: ", round(bestScoreSoFar, 4))

    trace[[traceIndex]] <- list(attrEncoding = currentAttrEncoding,
                                optimizationValue = bestScoreSoFar)

    iteration <- iteration + 1
    traceIndex <- traceIndex + 1

    firstStepResult <- firstStep(attributes, currentAttrEncoding,
                                 evaluationFunction, allowParallel, ...)

    # If there's no more neighbors to visit or the best
    # score from the neighbors is not good enough, the
    # current best solution is returned
    if (length(firstStepResult$neighborsOrderedByScore) == 0 ||
        bestScoreSoFar >= firstStepResult$orderedScores[1]) {

      solution <- attributes[as.logical(currentAttrEncoding)]

      return(list(
        solution = solution,
        trace = trace))
    }

    # Update the current best solution
    currentAttrEncoding <- firstStepResult$neighborsOrderedByScore[[1]]
    bestScoreSoFar <- firstStepResult$orderedScores[1]

    # ----------------------- Step 2 -------------------------------------
    # ----------- Conditional Exclusion for sffs -------------------------
    # ----------- Conditional Inclusion for sfbs  ------------------------

    # loop which implements the backtracking behavior of the algorithm
    while (TRUE) {

      if (verbose)
        message(" Conditional ", conditionalStepDescription, " Step ",
                " | Current Encoding: ", paste(currentAttrEncoding, collapse = ""),
                " | Optimization Value: ", round(bestScoreSoFar, 4),
                "\n-----------------------------------------------------------------------------")

      trace[[traceIndex]] <- list(attrEncoding = currentAttrEncoding,
                                  optimizationValue = bestScoreSoFar)

      traceIndex <- traceIndex + 1

      conditionalStepResult <- conditionalStep(attributes, currentAttrEncoding,
                                               evaluationFunction, allowParallel, ...)

      # If there's no neighbors to evaluate, stop conditional step
      if (length(conditionalStepResult$neighborsOrderedByScore) == 0)
        break

      # Extract best score from backtracked neighbors
      bestBacktrackScore <- conditionalStepResult$orderedScores[1]

      # If the best score from backtrack is worse than the current best,
      # stop conditional step
      if (bestScoreSoFar >= bestBacktrackScore)
        break

      # Update the current best solution
      currentAttrEncoding <- conditionalStepResult$neighborsOrderedByScore[[1]]
      bestScoreSoFar <- conditionalStepResult$orderedScores[1]
    }
  }

  stop("Should never reach this line...")
}

createSequentialSearchStepFunc <- function(type = c("sfs", "sbs")) {

  type <- match.arg(type)

  stepFunc <- function(attributes,
                       currentAttrEncoding,
                       evaluationFunction,
                       allowParallel,
                       ...) {

    sequentialSearchStep(
      attributes,
      currentAttrEncoding,
      evaluationFunction,
      type = type,
      allowParallel = allowParallel,
      ...)
  }

  return(stepFunc)
}
