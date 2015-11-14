
sfs <- function(attributes,
                evaluationFunction,
                verbose = TRUE,
                allowParallel = TRUE,
                ...) {

  searchResult <- sequentialSearch(
    attributes,
    evaluationFunction,
    type = "sfs",
    verbose = verbose,
    allowParallel = allowParallel,
    ...)

  return(searchResult)
}
