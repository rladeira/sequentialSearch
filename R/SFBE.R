
SFBE <- function(attributes,
                 evaluationFunction,
                 verbose = TRUE,
                 ...) {

  searchResult <-
    sequentialFloatingSearch(
      attributes = attributes,
      evaluationFunction = evaluationFunction,
      type = "SFBE",
      verbose = verbose,
      ...
    )

  return(searchResult)
}
