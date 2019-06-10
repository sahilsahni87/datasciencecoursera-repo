
## x is an invertible matrix
## return: a list containing functions to
##              1. set the matrix
##              2. get the matrix
##              3. set the inverse
##              4. get the inverse
##         this list is used as the input to cacheSolve()

makeCacheMatrix <- function(x = matrix()) {
  inverse = NULL
  set = function(y) {
 
    x <<- y
    inverse <<- NULL
  }
  get = function() x
  setinv = function(inverse1) inverse <<- inverse1
  getinv = function() inverse
  list(set=set, get=get, setinv=setinv, getinv=getinv)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse = x$getinv()
  
  
  if (!is.null(inverse)){
   
    message("get cached data")
    return(inverse)
  }
  
  # otherwise, calculate the inverse 
 data = x$get()
  inverse = solve(data, ...)
  
  # sets the value of the inverse in the cache via the setinv function.
  x$setinv(inverse)
  
  return(inverse)
}

