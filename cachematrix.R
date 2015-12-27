## Inverting a matrix can take up time and computing power, to lessen these effects we can 
## cache its solution.  The following functions show examples of this process.

## The first function takes the input, an invertible matrix, and creates a special object
## which is a list containing function to set the value of the matrix, get the value of
## the matrix, set the value of the inverse matrix, and get the value of the inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
  
  inv = NULL
  set = function(y) { 
    x <<- y
    inv <<- NULL
  }
  get = function() x
  setinv = function(inverse) inv <<- inverse 
  getinv = function() inv
  list(set=set, get=get, setinv=setinv, getinv=getinv)


}


## This function takes the output from the makeCacheMatrix and tests to see if the inverse is 
## stored in the 'inv' obejct.  If it is, it returns the cached solution.  If 'inv' is null, then 
## it calculates the inverse and stores it in 'inv'.

cacheSolve <- function(x, ...) {
  inv = x$getinv()
  
  if (!is.null(inv)){ 
    message("getting cached data")
    return(inv)
  }
   
  mat.data = x$get()
  inv = solve(mat.data, ...)
  
  x$setinv(inv)
  
  return(inv)
}
