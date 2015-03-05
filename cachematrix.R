## The following two functions enable creation of a special matrix object
## that supports caching of its inverse. That is, once the inverse of the matrix
## has been calculated, it is stored within the object, ready to be returned
## if the inverse is needed again, so that recomputation is not necessary
## If the matrix changes, the stored inverse is cleared, and is later recomputed
## if needed

## This function creates the matrix object, which is essentially a list 
## containing four functions that can be used to
## 1. set the matrix
## 2. get the matrix
## 3. set the cached inverse
## 4. get the cached inverse
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This function calculates the inverse of a matrix represented in the object
## form returned by the makeCacheMatrix function. If possible, it will reuse
## a cached inverse. If no such cached inverse exists, it will calculate the
## inverse and store it within the matrix object.
cacheSolve <- function(x) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getInverse()
    if(!is.null(inv)) {
        message("Using cached matrix inverse")
        return(inv)
    }
    mat <- x$get()
    inv <- solve(mat)
    x$setInverse(inv)
    inv
}
