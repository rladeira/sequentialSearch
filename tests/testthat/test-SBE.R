
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

