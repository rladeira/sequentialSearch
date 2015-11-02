
SFBE <- function(attributes,
                 evaluationFunction, 
                 verbose = TRUE,
                 isHigherBetter = TRUE,
                 ...) {
  
  searchResult <-
    sequentialFloatingSearch(
      attributes = attributes,
      evaluationFunction = evaluationFunction, 
      type = "SFBE",
      verbose = verbose,
      isHigherBetter = isHigherBetter,
      ...
    )
  
  return(searchResult)
}