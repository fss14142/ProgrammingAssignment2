## The two functions in this code are designed to cache the inverse of a matrix.
## To achieve this, the makeCacheMatrix creates a special kind of matrix
## object that can cache its inverse.
## The inverse of the matrix is computed only once, when the cacheSolve 
## function is called. The following calls to this function will return the 
## cached inverse.

## The makeCacheMatrix takes a standard R matrix object, and returns a list of 
## four functions (set, get, setInverse, getInverse).
## The set and get functions allow to create or access the matrix object.
## setInverse and getInverse allow to create or access the inverse of the matrix.


rm(list=ls())


makeCacheMatrix <- function(x = matrix()) {
    m <- NULL    
    #  The inverse of the matrix  will eventually be assigned to m. 
    # And this variable will keep the cached inverse.
    
    # The set function can be used to set the value of the matrix directly.
    # This will cause the value of m to be set to NULL.
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    
    # The get function returns the actual matrix.
    get <- function() x
    
    # The setInverse function can be called to assign
    # the computed inverse to the variable m.
    setInverse <- function(Inverse) m <<- Inverse
    
    
    # The getInverse function simply returns the stored inverse value.
    getInverse <- function() m
    
    
    # And this list of 4 functions is the returned value of makeCacheMatrix.
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)

} # End of makeCacheMatrix function code.



## The cacheSolve function takes a "special matrix" created with makeCacheMatrix
## and checks to see if the inverse is already stored in the cache. If it is, 
## the inverse is returned. Otherwise, the inverse is obtained and returned.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    ## First we check the cache, calling getInverse.
    m <- x$getInverse()
    
    ## if the inverse is already stored in the cache...
    if(!is.null(m)) {
        message("getting cached data")
        ## ... the cached value is returned.       
        return(m) 
        ## execution of cacheSolve ends here in this case.
    }
    
    ## But if the cached inverse is NULL, then we need to get the matrix and
    ## call solve to compute the inverse.
    data <- x$get()
    m <- solve(data, ...)
    ## We call setInverse to store the inverse in the cache for future calls.
    x$setInverse(m)
    ## and the inverse is returned.
    return(m)

}  ## End of cacheSolve code.



## Example. Uncomment to use:
 
# A = makeCacheMatrix(matrix(c(2,1,5,3),nrow=2)  )
# cacheSolve(A)  # First call to cacheSolve computes and returns the inverse.
# cacheSolve(A)  # A new call uses the cached inverse and displays a message. 

