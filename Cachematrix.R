makeCacheMatrixr <- function(x = matrix()) { #We take input in Matrix form only
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() {x}
  setinv <- function(inverse) {inv <<- inverse}
  getinv <- function() {inv}
  
}

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}