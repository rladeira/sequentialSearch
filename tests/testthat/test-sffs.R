
context("sffs")

test_that("sffs behaves as expected 1", {

  dummyEvaluation <- function(attrEncoding, ...) {
    return(length(attrEncoding))
  }

  data <- iris[, -5]
  attributes <- colnames(data)

  result <- sffs(attributes = attributes,
                 evaluationFunction = dummyEvaluation,
                 allowParallel = FALSE)

  expect_true(all.equal(result$solution, attributes))
  expect_true(length(result$trace) == 2*length(attributes) + 1)
})

test_that("sffs behaves as expected 2", {

  dummyEvaluationNegative <- function(attrEncoding, ...) {
    return(-1 * length(attrEncoding))
  }

  data <- iris[, -5]
  attributes <- colnames(data)

  result <- sffs(attributes = attributes,
                 evaluationFunction = dummyEvaluationNegative)

  expect_true(length(result$solution) == 0)
  expect_true(length(result$trace) == 1)
})

test_that("sffs behaves as expected 3", {

  require(rDatasets)
  require(clusterCrit)

  evalFunc <- function(attributes, dataset) {

    if (length(attributes) == 0)
      return(-Inf)

    featureSubset <- dataset$X[, attributes, drop = FALSE]

    criterion <- intCriteria(featureSubset,
                             as.integer(dataset$Y),
                             "Silhouette")

    return(criterion$silhouette)
  }

  attributes <- colnames(iris_$X)

  result <- sffs(attributes = attributes,
                 evaluationFunction = evalFunc,
                 dataset = iris_)

  expectedSolutionSize <- (length(result$trace) - 1) / 2

  expect_true(length(result$solution) == expectedSolutionSize)
})

