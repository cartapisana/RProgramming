## Joseph W. Grubbs (Coursera Email: joseph.grubbs@outlook.com)
## R Programming Week 3 Peer-Graded Programming Assignment
## Source: "ProgrammingAssignment2/cachematrix.R

## Create matrix object "makeCacheMatrix" to cache its inverse
## Assume given matrix persists as invertible
makeCacheMatrix <- function(x = matrix()) {
  
    ## Set inverse "inv" to NULL as placeholder
    ## Define "set" function to reassign matrix from "x" to "y"
    ## Reset inverse "inv" to NULL
    inv = NULL
    set = function(y) {
        x <<- y
        inv <<- NULL
    }
    ## Define "get" function to return matrix "x"
    ## Set "inv" to "inverse"
    ## Define "getinv" function to return inverse "inv"
    ## Specify list to return matrix object with defined functions
    get = function() x
    setinv = function(inverse) inv <<- inverse 
    getinv = function() inv
    list(set = set, get = get, 
         setinv = setinv, 
         getinv = getinv)
}

## Define "cacheSolve" function to compute inverse of "makeCacheMatrix"
cacheSolve <- function(x, ...) {
    inv = x$getinv()
  
    ## Test with "if" to determine whether "inv" has been calculated
    if (!is.null(inv)) {
    ## If "inv" not calculated retrieve from cache, return cached value
      message("Just a moment, getting cached data...")
      return(inv)
  }
  
    ## If "inv" not calculated, calculate inverse value
    mat.data = x$get()
      inv = solve(mat.data, ...)
  
    ## Define "setinv" function to set inverse value in cache
    x$setinv(inv)
  
    ## Return inverse of "x" as a matrix
    return(inv)
}