
sfbs <- function(attributes,
                 evaluationFunction,
                 verbose = TRUE,
                 allowParallel = TRUE,
                 ...) {

  searchResult <- sequentialFloatingSearch(
    attributes,
    evaluationFunction,
    type = "sfbs",
    verbose = verbose,
    allowParallel = allowParallel,
    ...)

  return(searchResult)
}
