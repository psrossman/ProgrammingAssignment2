## Matrix inversion is typically a costly computation and there may be some benefit
## to caching the inverse of a matrix rather than compute it repeatedly. The
## following functions are used to cache the inverse of a matrix.

## makeCacheMatrix creates a list of functions to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of inverse of the matrix
## 4. get the value of inverse of the matrix


makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x;
  setInverse <- function(inverse) inv <<- inverse;
  getInverse <- function() inv;
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## The following function returns the inverse of the matrix. It first checks if
## the inverse has already been computed in the function above. If so, it gets the result and skips the
## computation. If not, it computes the inverse, sets the value in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse();
        if (!is.null(inv)) {
                message("getting cached data");
                return(inv);
        }
        mat <- x$get();
        inv <- solve(mat, ...);
        x$setInverse(inv);
        inv;
}
