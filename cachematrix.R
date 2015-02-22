makeCacheMatrix <- function(x = matrix()) {
  inverted <- NULL
  set <- function(y) {
    x <<- y
    inverted <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inverted <<- inverse
  getinverse <- function() inverted
  list(set=set, get=get, 
       setinverse=setinverse, 
       getinverse=getinverse)
}


# The following function returns the inverse of the matrix. It first checks if
# the inverse has already been computed. If so, it gets the result and skips the
# computation. If not, it computes the inverse, sets the value in the cache via
# setinverse function.

# This function assumes that the matrix is invertible.
cacheSolve <- function(x, ...) {
  matInv <- x$getinverse()  ## try to get inverted Matrix from cache
  if(!is.null(matInv)) {   ##is.na gives error message so switched to NULL 
    message("getting cached data.")
    return(matInv)
  } 
  data <- x$get()
  ##Use solve to determine inverse of Matrix if inverse not calculated earlier
  matInv <- solve(data)
  x$setinverse(matInv)   ##store the inverse so inverse will not need to be calculated next time
  matInv
}
