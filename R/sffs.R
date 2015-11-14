
sffs <- function(attributes,
                 evaluationFunction,
                 verbose = TRUE,
                 allowParallel = TRUE,
                 ...) {

  searchResult <- sequentialFloatingSearch(
    attributes,
    evaluationFunction,
    type = "sffs",
    verbose = verbose,
    allowParallel = allowParallel,
    ...)

  return(searchResult)
}
