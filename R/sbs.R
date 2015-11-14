
sbs <- function(attributes,
                evaluationFunction,
                verbose = TRUE,
                allowParallel = TRUE,
                ...) {

  searchResult <- sequentialSearch(
    attributes,
    evaluationFunction,
    type = "sbs",
    verbose = verbose,
    allowParallel = allowParallel,
    ...)

  return(searchResult)
}
