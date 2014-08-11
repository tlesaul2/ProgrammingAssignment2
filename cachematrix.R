## These functions create a "cacheMatrix" object capable of caching its inverse
## once calculated
## Technically, calling makeCacheMatrix() returns a list object containing named
## auxilliary functions whose scopes also contain the "matrix" passed in.  The
## matrix can be reassigned, retrieved, or have its inverse set or retrieved
## using the auxilliary functions.

## Create "cacheMatrix" object with and auxilliary functions

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL             ## initialize inv as unknown
    set <- function(y){
        x <<- y             ## assign y to x in parent scope
        inv <<- NULL        ## ensure inv is NULL in parent scope
    }
    get <- function() x     ## evaluate to return
    setinverse <- function(inverse) inv <<- inverse  ## asign in parent scope
    getinverse <- function() inv  ## evaluate to return
    list(set = set, get = get,    ## lastly, evaluate a list with the auxilliary
         setinverse = setinverse, ## functions named.  When a cacheMatrix is
         getinverse = getinverse) ## created, its auxilliary functions can be
                                  ## accessed by "Subsetting" the returned list
}


## Use auxilliary functions to retrieve the inverse of x, calculating and
## caching it if is still unknown

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()               ## retrieve inverse
    if(!is.null(inv)) {                 ## test if its known
        message("getting cached data")  ## message
        return(inv)                     ## explicitly return
    }
    inv <- solve(x$get())               # retrieve matrix and find inverse
    x$setinverse(inv)                   # cache inverse
    inv                                 # evaluate to return
}
