## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
      inv <- matrix()

      set <- function(y) {
              x <<- y
              inv <<- matrix()
      }

      get <- function() x
	  
      setinv <- function(solve) inv <<- solve ## set inverse of the matrix
	  
      getinv <- function() inv ## get inverse of the matrix

	list(set = set, get = get, setinv = setinv, getinv = getinv) # return vector type list with all functions

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
        
	inv <- x$getinv() ## get inverse matrix data

	## if the inverse is not empty, get the inverse matrix data, otherwise do solve
        if(length(inv)>1) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv

}
