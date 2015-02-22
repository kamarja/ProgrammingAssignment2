## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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

## Write a short comment describing this function
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
