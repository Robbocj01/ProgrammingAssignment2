## This function creates a special "matrix" object that can cache its inverse

## The first function, makeVector creates a special "vector.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- solve(x)
    getinverse <- function() m
    list(set = set, 
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)
  
}

##  function calculates the mean of the special "vector" created with the above function.
## However, it first checks to see if the mean has already been calculated. 
## If so, it gets the mean from the cache and skips the computation

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}



