## [Shashi K Ranjan || Programming Assignment 2: Lexical Scoping

## SUMMARY
# makeCacheMatrix will create the special matrix object and the get, set functions. cacheSolve will compute and cache the inverse.


## CODE STARTS HERE
# makeCacheMatrix function creates a matrix object that can cache its own inverse matrix.
# x is the matrix whose inverse needs to be cached.
# inv will store the inverse of x.

makeCacheMatrix <- function(x = matrix()) {
    # set initial value of NULL to inv as the inverse is not computed yet.
    inv <- NULL
    
    # set function will allow us to set a matrix value for this special matrix object.
    set <- function( y){
        x <<- y
        inv <<- NULL
    }
    
    # get function will return the matrix stored.
    get <- function() x
    
    # setInverse will cache the passed argument as the inverse of this matrix object.
    setInverse <- function( inverse){ 
        inv <<- inverse
    }
    
    # function to return the inverse matrix cached
    getInverse <- function() inv
    
    list( set = set, get = get,
        setInverse = setInverse,
        getInverse = getInverse)
}

# cachSolve function checks if the inverse of the matrix passed as argument is already cached, and 
# returns the same if yes. Otherwise, it computes the inverse of x, and uses the setInverse() function
# to cache it.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    # Check if the inverse is already cached. Return the cached result if yes.
    inv <- x$getInverse()
    if( !is.null(inv)){
        message( "Inverse already cached. Getting cached data...")
        return( inv)
    }
    
    # If not already cached, compute the inverse matrix, and cache it using the setInverse function
    matrix_vector <- x$get()
    inv <- solve( matrix_vector)
    x$setInverse( inv)
    
    # Return the computed inverse matrix
    inv
}
