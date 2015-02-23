## These two functions are used together to cache and compute the inverse of a matrix.
## 

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmatrix <- function(mtrix) m <<- solve(mtrix)
        getmatrix <- function() m
        list(set = set, get = get,
             setmatrix = setmatrix,
             getmatrix = getmatrix)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix.
cacheSolve <- function(x, ...) {
  ## Return a precalculated matrix that is the inverse of 'x' if available
  m <- x$getmatrix()

  ## Return cached value
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }

  ## Calculate the inverse if not cached and add to cache using setmatrix
  data <- x$get()
  m <- solve(data, ...)
  x$setmatrix(m)
  m
}
