
SFS <- function(attributes,
                evaluationFunction, 
                verbose = TRUE,
                isHigherBetter = TRUE,
                ...) {
  
  searchResult <-
    sequentialSearch(
      attributes,
      evaluationFunction,
      type = "SFS",
      verbose = verbose,
      isHigherBetter = isHigherBetter,
      ...)
  
  return(searchResult)
}