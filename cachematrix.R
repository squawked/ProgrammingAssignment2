##  makeCacheMatrix and cacheSolve allows a square matrix inverse value to be cached

## makeCacheMatrix creates a special matrix that allows storing and retrieving the inverse value

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve calculates the inverse of the special matrix. 
## It first checks if the inverse had previously been calculated and returns that value if it has.
## Otherwise it calculates the inverse and stores the inverse value

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
         i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
