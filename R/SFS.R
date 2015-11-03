
SFS <- function(attributes,
                evaluationFunction,
                verbose = TRUE,
                ...) {

  searchResult <-
    sequentialSearch(
      attributes,
      evaluationFunction,
      type = "SFS",
      verbose = verbose,
      ...)

  return(searchResult)
}
