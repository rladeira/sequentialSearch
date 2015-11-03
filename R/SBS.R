
SBS <- function(attributes,
                evaluationFunction,
                verbose = TRUE,
                ...) {

  searchResult <-
    sequentialSearch(
      attributes,
      evaluationFunction,
      type = "SBS",
      verbose = verbose,
      ...)

  return(searchResult)
}
