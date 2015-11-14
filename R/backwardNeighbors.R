
backwardNeighbors <- function(attrEncoding) {

  neighbors <-
    lapply(1:length(attrEncoding),
           function (i) {
             if (attrEncoding[i] == 0)
               return(invisible())

             neighbor <- attrEncoding
             neighbor[i] <- 0
             return(neighbor)
           })

  return(Filter(Negate(is.null), neighbors))
}
