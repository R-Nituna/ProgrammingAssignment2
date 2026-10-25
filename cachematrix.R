
## Create a special matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL

    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }

    get <- function() {
        x
    }

    setInverse <- function(solveMatrix) {
        inverse <<- solveMatrix
    }

    getInverse <- function() {
        inverse
    }

    list(
        set = set,
        get = get,
        setInverse = setInverse,
        getInverse = getInverse
    )
}

## Compute the inverse, using the cache when available
cacheSolve <- function(x, ...) {
    inverse <- x$getInverse()

    if (!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }

    data <- x$get()
    inverse <- solve(data, ...)
    x$setInverse(inverse)

    inverse
}
