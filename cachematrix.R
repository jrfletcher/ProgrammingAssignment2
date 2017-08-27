## makeCacheMatrix is a function that contains a matrix that will cache its inverse
##
## i is the inverse of the matrix
##
## set enables setting the matrix to be solved and resets the i variable since the 
## solve function needs to be executed again
##
## get returns the matrix
##
## setinverse sets the inverse of the matrix (the i variable)
##
## getinverse returns the inverse of the matrix 
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function
##
## gets the solve and if it's not null the solve is returned
## if the solve has not been set then it is set and returned
cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
