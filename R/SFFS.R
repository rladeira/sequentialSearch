
SFFS <- function(attributes,
                 evaluationFunction,
                 verbose = TRUE,
                 ...) {

  searchResult <-
    sequentialFloatingSearch(
      attributes = attributes,
      evaluationFunction = evaluationFunction,
      type = "SFFS",
      verbose = verbose,
      ...)

  return(searchResult)
}
