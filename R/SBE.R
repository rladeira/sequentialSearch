
SBE <- function(attributes,
                evaluationFunction,
                verbose = TRUE,
                ...) {

  searchResult <-
    sequentialSearch(
      attributes,
      evaluationFunction,
      type = "SBE",
      verbose = verbose,
      ...)

  return(searchResult)
}
