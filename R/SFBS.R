
SFBS <- function(attributes,
                 evaluationFunction,
                 verbose = TRUE,
                 ...) {

  searchResult <-
    sequentialFloatingSearch(
      attributes = attributes,
      evaluationFunction = evaluationFunction,
      type = "SFBS",
      verbose = verbose,
      ...
    )

  return(searchResult)
}
