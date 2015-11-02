
SBE <- function(attributes,
                evaluationFunction, 
                verbose = TRUE,
                isHigherBetter = TRUE,
                ...) {
  
  searchResult <-
    sequentialSearch(
      attributes,
      evaluationFunction,
      type = "SBE",
      verbose = verbose,
      isHigherBetter = isHigherBetter,
      ...)
  
  return(searchResult)
}