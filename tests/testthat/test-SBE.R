
context("SBE")

test_that("SBE behaves as expected 1", {

  dummyEvaluation <- function(attrEncoding, ...) {
    return(length(attrEncoding))
  }

  data <- iris[, -5]
  attributes <- colnames(data)

  result <- SBE(attributes = attributes,
                evaluationFunction = dummyEvaluation)

  expect_true(all.equal(result$solution, attributes))
  expect_true(length(result$trace) == 1)
})

test_that("SBE behaves as expected 2", {

  dummyEvaluationNegative <- function(attrEncoding, ...) {
    return(-1 * length(attrEncoding))
  }

  data <- iris[, -5]
  attributes <- colnames(data)

  result <- SBE(attributes = attributes,
                evaluationFunction = dummyEvaluationNegative)

  expect_true(length(result$solution) == 0)
  expect_true(length(result$trace) == length(attributes) + 1)
})

test_that("SBE behaves as expected 3", {

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

  result <- SBE(attributes = attributes,
                evaluationFunction = evalFunc,
                dataset = iris_)

  expectedSolutionSize <-
    length(attributes) - length(result$trace) + 1

  expect_true(length(result$solution) == expectedSolutionSize)
})

test_that("SBE behaves as expected with no execution trace.", {

  dummyEvaluation <- function(attrEncoding, ...) {
    return(length(attrEncoding))
  }

  data <- iris[, -5]
  attributes <- colnames(data)

  solution <- SBE(attributes = attributes,
                  evaluationFunction = dummyEvaluation,
                  traceExecution = FALSE)

  expect_true(all.equal(solution, attributes))
})
