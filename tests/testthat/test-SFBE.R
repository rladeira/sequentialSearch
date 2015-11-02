
context("SFBE")

test_that("SFBE behaves as expected 1", {
  
  dummyEvaluation <- function(attrEncoding, ...) {
    return(sum(attrEncoding))
  }
  
  data <- iris[, -5]
  attributes <- colnames(data)
  
  result <- SFBE(attributes = attributes,
                evaluationFunction = dummyEvaluation)
  
  expect_true(all.equal(result$solution, attributes))
  expect_true(length(result$trace) == 1)
})

test_that("SFBE behaves as expected 2", {
  
  dummyEvaluationNegative <- function(attrEncoding, ...) {
    return(-1 * sum(attrEncoding))
  }
  
  data <- iris[, -5]
  attributes <- colnames(data)
  
  result <- SFBE(attributes = attributes,
                evaluationFunction = dummyEvaluationNegative)
  
  expect_true(length(result$solution) == 0)
  expect_true(length(result$trace) == 2*length(attributes) + 1)
})

