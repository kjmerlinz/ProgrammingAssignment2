## The following functions can be used to calculate and cache
## the inverse of a matrix.
##
## If the matrix is not changing then the inverse can be looked
## up in the cache rather than being recalculated each time it
## is needed.

## makeCacheMatrix creates a special "matrix", which is really a list 
## containing a function to
## 1) set the value of the matrix
## 2) get the value of the matrix
## 3) set the value of the inverse
## 4) get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    
    inv <- NULL  # inverse is initially NULL
    set <- function(y) {  # used when changing to a new matrix
        x <<- y
        inv <<- NULL
    }
    get <- function() x  # return the value of the matrix
    setinv <- function(solve) inv <<- solve  # calculate the inverse
    getinv <- function() inv  # return the inverse
    list(set = set, get = get, # list contining the above functions
         setinv = setinv,
         getinv = getinv)
    
}


## cacheSolve finds the inverse of the special "matrix" created above.
## It first checks to see if the inverse has already been calculated.
## If so, it gets the inverse from the cache and skips the computation.
## Otherwise, it calculates the inverse and sets the value in the cache
## via the setinv function.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()  # Try to retreive the inverse
    if(!is.null(inv)) {  
        message("getting cached data")
        return(inv)  # If something was obtained above, return it.
    }
    data <- x$get()  # Otherwise, get the matrix
    inv <- solve(data, ...)  # Calculate the inverse
    x$setinv(inv)  # and set it into the cache
    inv  # finally return the inverse
}