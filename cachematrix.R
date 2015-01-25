## These two functions work together to cache a matrix based on user-input, 
## and solve the inverse of that matrix if is new, or return the previous inverse from the cache if it has not changed.

## This creates a matrix from the user-input, defines a function (getinverse) to retrieve that matrix, and 
## sets i to null to indicate to cacheSolve that the cache has been updated. It is not designed for input or output 
## on its own, but rather as a sub-routine of cacheSolve.

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

## This function checks if i above is not null (indicating it has been reset) and if it is not, returns the same value. 
## If it detects i is null, it computes the inverse. This effectively prevents a computation if the matrix has not been changed.

cacheSolve <- function(x, ...) {
  i2 <- x$getinverse()
  if(!is.null(i2)) {
    message("getting cached data")
    return(i2)
  }
  data <- x$get()
  i2 <- solve(data, ...)
  x$setinverse(i2)
  i2
}

        ## Return a matrix that is the inverse of 'x'
