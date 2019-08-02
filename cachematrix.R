## makeCacheMatrix to set/get value of the matrix and inverse of it
## cacheSolve calculates the inverse of a matrix

## makeCacheMatrix has four get and set functions inside it for getting/setting the matrix
## and inverse of it

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setinvert <- function(solve) m <<- solve
    getinvert <- function() m
    list(set = set, get = get,
         setinvert = setinvert,
         getinvert = getinvert)
}


## cacheSolve calculates the inverse of a matrix but it checks if the inverse exists
## before making the calculation

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinvert()
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinvert(m)
    m
}
