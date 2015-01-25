## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    ## create initial cache for inverse
    inv <- NULL 
    ## set the value of the matrix
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    ## get the value of the matrix
    get <- function() x 
    ## set the value of the inverse matrix
    setinverse <- function(solve) inv <<- solve
    ## get the value of the inverse matrix
    getinverse <- function() inv 
    ## for the list for function
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)

}


## Write a short comment describing this function
## This function computes this inverse of the special "matrix" returned by makeCacheMatrix above. if the inverse has already been calculated (and the matrix has not been changed, then cacheSolve should retrieve the inverse from the cache)
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## retrieve the cache value
        inv <- x$getinverse() 
        ## return non null cache value
        if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
        } 
        ## get the matrix
        data <- x$get() 
        ## calculate the matrix inverse 
        m <- solve(data, ...) 
        ## store the cache value
        x$setinverse(inv) 
        ## print the inverse matrix
        inv 
}
