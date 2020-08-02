## File have 2 functions using objects

## This function create a matrix object that has 4 methods and a constructor: 
## set, get, set_InvMatrix and get_InvMatrix. The input object must be of 
## class matrix(). The once the inverse matrix is calculated it's cache in the object. 

makeCacheMatrix <- function(x = matrix()) {
  inv_matrix <- NULL
  set <- function(y) {
    x <<- y
    inv_matrix <<- NULL
  }
  get <- function() x
  set_InvMatrix <- function(inv) inv_matrix <<- inv
  get_InvMatrix <- function() inv_matrix
  list(set = set, get = get,
       set_InvMatrix = set_InvMatrix,
       get_InvMatrix = get_InvMatrix)
}


## This function calculate the inverse matrix of the object create by the 
## function makeCacheMatrix. If the object already has a inverse matrix 
## skip the calculation and just recover the inverse matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$get_InvMatrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$set_InvMatrix(m)
  m
}
