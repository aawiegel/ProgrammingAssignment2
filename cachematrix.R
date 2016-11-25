## Creates a methods to set and get a matrix and its inverse.
## These methods are used with cacheSolve to avoid costly
## calculations of the inverse of the matrix by cacheing the results

## Creates methods to set and get a matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
     # Initially leaves the inverse as a null value
     i <- NULL
     
     # Sets the value of the matrix and resets the value of the inverse
     set <- function(y) {
          x <<- y
          i <<- NULL
     }
     
     # Gets the value of the matrix
     get <- function() x
     
     # Sets the value of the inverse of the matrix
     setInv <- function(Inv) i <<- Inv
     
     # gets the value of the inverse of the matrix
     getInv <- function() i
     
     # returns a list which refers to each of these functions
     list(set = set, get = get,
          setInv = setInv,
          getInv = getInv)
}

## Calculate the inverse of a matrix if it has not been yet.
## Returns the cached value of the inverse if it has already been calculated
## NOTE: does not check if the matrix is invertable

cacheSolve <- function(x, ...) {
     # Get the value of the inverse
     i <- x$getInv()
     
     # If the inverse has already been calculated, return the inverse.
     if(!is.null(i)) {
          message("getting cached data...")
          return(i)
     }
     
     # If the inverse has not been calculated,
     # find it, set it to x, and return it.
     data <- x$get()
     i <- solve(data, ...)
     x$setInv(i)
     i
}
