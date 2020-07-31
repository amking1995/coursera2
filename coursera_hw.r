
#Answer 1

#For this, we cache the  Inverse by creating a special function

makeCacheMatrix <- function(x = matrix()) {
inv <- NULL
  set <- function(y){
  x <<- y
  inv <<- NULL
  }
  get <- function()x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, get = get,
  setInverse = setInverse,
  getInverse = getInverse)
}

# With the function that we have created, it will compute the inverse of the special "matrix" returned by the function from above. 
# If we have calculated the inverse, then the cachesolve should get the inverse. We can then compute the inverse of the square matrix using R.

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of x
  inv <- x$getInverse()
  if(!is.null(inv)){
  message("getting cached data")
  return(inv)
  }
  mat <- x$get()
  inv <- solve(mat,...)
  x$setInverse(inv)
  inv
}
