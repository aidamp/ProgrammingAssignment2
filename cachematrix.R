## Catching the Inverse of a Matrix

## The makeCacheMatrix function will set and get the value of a matrix. 
## Also, it will get and set the inverse of the matrix when needed.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y){
        x <<- y
        i <<- NULL
    }
    get <- function()x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set,
         get = get,
         setinverse = setinverse,
         getinverse = getinverse
         )
}


## The following function calculates the inverse of the matrix created with the 
## makeCacheMatrix function. If the calculation already exists it will get the
## inverse from the cache data, otherwise it will calculate it.

cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if (!is.null(i)){
        message("getting cached inverse of a matrix")
        return (i)
    }
    data <- x$get()
    i <- solve(data,...)
    x$setinverse(i)
    i
}

