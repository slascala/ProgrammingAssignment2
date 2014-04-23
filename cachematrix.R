### This function will calculate and store the inverse of a matrix in a cache matrix 
##
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}
 

 

## Return a matrix that is the inverse of 'x'
## it retruns the cached matrix if it exists otherwise it calculates the inverse and stores
## it via the cache matrix object
cacheSolve <- function(x, ...) {


        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
 

 
