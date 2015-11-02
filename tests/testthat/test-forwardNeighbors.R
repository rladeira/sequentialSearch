
context("forwardNeighbors")

test_that("forwardNeighbors behaves as expected for empty encoding", {
  
  emptyAttrEncoding <- rep(0, 100)
  
  neighbors <- forwardNeighbors(emptyAttrEncoding)
  
  for (i in 1:length(neighbors)) {
    neighbor <- neighbors[[i]]
    expect_true(neighbor[i] == 1) 
  }
})

test_that("forwardNeighbors behaves as expected for full attributes encoding", {
  
  fullAttrEncoding <- rep(1, 1000)
  
  neighbors <- forwardNeighbors(fullAttrEncoding)
  expect_true(length(neighbors) == 0)
})

test_that("forwardNeighbors behaves as expected for partial attributes encoding", {
  
  attrEncoding <- sample(c(0, 1), 100, replace = TRUE)
  nZeros <- sum(attrEncoding == 0)
  zerosIndexes <- which(attrEncoding == 0)
  
  neighbors <- forwardNeighbors(attrEncoding)
  
  expect_true(length(neighbors) == nZeros)
  
  for (i in 1:length(neighbors)) {
    neighbor <- neighbors[[i]]
    expect_true(neighbor[zerosIndexes[i]] == 1) 
  }
})


