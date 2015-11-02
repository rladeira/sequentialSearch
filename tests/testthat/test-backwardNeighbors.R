
context("backwardNeighbors")

test_that("backwardNeighbors behaves as expected for empty encoding", {
  
  emptyAttrEncoding <- rep(0, 1000)
  
  neighbors <- backwardNeighbors(emptyAttrEncoding)
  expect_true(length(neighbors) == 0)
})

test_that("backwardNeighbors behaves as expected for full attributes encoding", {
  
  fullAttrEncoding <- rep(1, 100)
  
  neighbors <- backwardNeighbors(fullAttrEncoding)
  
  for (i in 1:length(neighbors)) {
    neighbor <- neighbors[[i]]
    expect_true(neighbor[i] == 0) 
  }
})

test_that("backwardNeighbors behaves as expected for partial attributes encoding", {
  
  attrEncoding <- sample(c(0, 1), 100, replace = TRUE)
  nOnes <- sum(attrEncoding == 1)
  onesIndexes <- which(attrEncoding == 1)
  
  neighbors <- backwardNeighbors(attrEncoding)
  
  expect_true(length(neighbors) == nOnes)
  
  for (i in 1:length(neighbors)) {
    neighbor <- neighbors[[i]]
    expect_true(neighbor[onesIndexes[i]] == 0) 
  }
})


