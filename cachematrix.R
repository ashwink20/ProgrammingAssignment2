## The following functions are defined to create a matrix object and calculate the inverse of the matrix
## taking advantage of lexical scoping of R and using cached values when available for faster execution

## This function creates the matrix object

makeCacheMatrix <- function(x = matrix()) {

    inv <- NULL
    
    set <- function(y) {
        x <<- y
        inv <<- NULL    #sets inverse to null when the matrix value is changed
    }
    
    get <- function() {
        x
    }
    
    setinverse <- function(inverse) inv <<- inverse
    
    getinverse <- function() inv
    
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
    
}


## This Function returns the inverse of the supplied matrix object. If inverse is already computed and available in cache, it returns cache value. Otherwise, the newly computer value is returned. 

cacheSolve <- function(x, ...) {
    
    inv <- x$getinverse()
    
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    
    inv
}
