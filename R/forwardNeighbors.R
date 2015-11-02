
forwardNeighbors <- function(attrEncoding) {
  
  neighbors <-
    lapply(1:length(attrEncoding),
           function (i) {
             if (attrEncoding[i] == 1)
               return(invisible())
             
             neighbor <- attrEncoding
             neighbor[i] <- 1
             return(neighbor)
           })
  
  return(Filter(Negate(is.null), neighbors))
}