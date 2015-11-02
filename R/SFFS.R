
SFFS <- function(attributes,
                 evaluationFunction, 
                 verbose = TRUE,
                 isHigherBetter = TRUE,
                 ...) {
  
  searchResult <-
    sequentialFloatingSearch(
      attributes = attributes,
      evaluationFunction = evaluationFunction, 
      type = "SFFS",
      verbose = verbose,
      isHigherBetter = isHigherBetter,
      ...
    )
  
  return(searchResult)
}