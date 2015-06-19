## These functions allow caching the inverse computation on a matrix
## Inputs are assumed to be a valid square matrix that is invertable
##
## To use the cached computation you must first create a special "matrix"
##  function that can cache its inverse.  This is done with the 
##  "makeCacheMatrix" function.
##   $set - sets the matrix you would like to cache the inverse of
##   $get - returns the matrix that was stored
##   $getInverse - returns the inverse of the matrix
##   $setInverse - caches the inverse of the matrix
##
##  "cacheSolve" computes or retrieves from cache the inverse of the matrix 
##    in the makeCacheMatrix object
##
##  Use:
##
##   m <- makeCacheMatrix(matrix(c(1,-1/4,-1/4,1), 2, 2))
##   im <- cacheSolve(m) # inverse of matrix m
##
##   all subsequent calls using m will retrieve the result from cache until 
##    m$set is called


## Create a new cachable matrix with this function
makeCacheMatrix <- function(x = matrix()) {
    # i stores the cached inverse
    i <- NULL
    set <- function(y) {
        x <<-y
        i <<- NULL
    }
    get <- function() x
    setInverse <- function(inv) i <<- inv
    getInverse <- function() i
    list(set = set, get = get, 
         setInverse = setInverse, 
         getInverse = getInverse)
}


## Returns the inverse x.  It will return a cached result if present or a
##  computed result (caching it for further calls)
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    i <- x$getInverse()
    if (!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data)
    x$setInverse(i)
    i
}
